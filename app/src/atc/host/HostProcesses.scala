package atc.host

import atc.{LauncherEnvironment, ScalaSource}
import atc.lib.*
import atc.perms.ScopeId
import atc.platform.{Platform, WindowsExecutable}

import java.lang.ProcessBuilder.Redirect
import java.nio.file.Path
import java.util.Locale
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** Command execution and spawned-process lifecycle supplied by [[Host]].
  *
  * This implementation module is compiled outside capture checking, so its
  * erased override signatures use plain capability types. The agent-facing
  * [[atc.lib.Interface]] contract requires full `Exec^` and `FileSystem^` for
  * `exec`, `execOutput`, and `spawn`.
  */
private[host] trait HostProcesses:
  self: Host =>

  def requestExec[T](commands: Iterable[String])(op: Exec ?=> T)(using UserIO, Exec): T =
    requestExec(commands, "")(op)

  def requestExec[T](commands: Iterable[String], reason: String)(op: Exec ?=> T)(using user: UserIO, parent: Exec): T =
    inScope(policy.requestExec(scopeOf(parent), commands.toList, reason))(id => op(using ExecImpl(id, envOf(parent))))

  /** The block's capability keeps its parent's scope: it grants nothing, since every
    * command it runs is authorized with its variables at use (`authorizeCommands`). */
  def withEnv[T](vars: Map[String, String])(op: Exec ?=> T)(using parent: Exec): T =
    for (name, value) <- vars do
      if !CommandLine.isVariableName(name) then
        throw IllegalArgumentException(
          s"withEnv: ${ScalaSource.stringLiteral(name)} is not a variable name (letters, digits and _, not starting with a digit)"
        )
      if value.contains('\u0000') then throw IllegalArgumentException(s"withEnv: the value of $name contains NUL")
    if Platform.isWindows then
      vars.keys.groupBy(_.toUpperCase(Locale.ROOT)).values.find(_.sizeIs > 1).foreach: names =>
        throw IllegalArgumentException(s"withEnv: ${names.toList.sorted.mkString(" and ")} are one variable on Windows")
    op(using ExecImpl(scopeOf(parent), withVariables(envOf(parent), vars)))

  private def envOf(ex: Exec): Map[String, String] = ex match
    case impl: ExecImpl => impl.env
    case _ => Map.empty

  /** `vars` over `base`, where a name replaces any spelling of itself that Windows treats as equal. */
  private def withVariables(base: Map[String, String], vars: Map[String, String]): Map[String, String] =
    val kept =
      if Platform.isWindows then base.filterNot((name, _) => vars.keys.exists(_.equalsIgnoreCase(name)))
      else base
    kept ++ vars

  def exec(command: String)(using Exec, FileSystem): ProcessResult = exec(command, Nil, ExecOptions())

  def exec(command: String, args: Seq[String])(using Exec, FileSystem): ProcessResult =
    exec(command, args, ExecOptions())

  def exec(command: String, args: Seq[String], workingDir: String)(using Exec, FileSystem): ProcessResult =
    exec(command, args, ExecOptions(workingDir = workingDir))

  /** A command line ready to start: parsed, authorized, and represented as one
    * `ProcessBuilder` per pipeline stage. Shared by `exec` and `spawn`. */
  private final case class Prepared(pbs: List[ProcessBuilder], stageLines: List[String], line: String)

  private def withArgs(command: String, args: Seq[String]): CommandLine.Pipeline =
    val pipeline = CommandLine.parsePipeline(command)
    if args.isEmpty then pipeline
    else if pipeline.isSimple then
      val stage = pipeline.stages.head
      pipeline.copy(stages = List(stage.copy(argv = stage.argv ++ args)))
    else
      throw IllegalArgumentException(
        "exec(command, args, ...): `command` must be one program when args are given; write a pipeline or redirection in the one-line form exec(\"...\")"
      )

  /** Every stage must be permitted by itself and, for each of the `assignments` it runs
    * with, as `NAME=value stage`. One variable at a time: a pattern's `*` could otherwise
    * reach over a second assignment, and a grant of `NAME=value` alone, a word prefix of
    * every such line, would permit any command. */
  private def authorizeCommands(pipeline: CommandLine.Pipeline, scope: ScopeId, assignments: List[String]): Unit =
    val lines = pipeline.stages.flatMap(stage => stage.line :: assignments.map(a => s"$a ${stage.line}"))
    for line <- lines; pattern <- policy.commandDenied(line) do
      throw SecurityException(
        s"Access denied: command '$line' is refused by the configuration (denyCommands pattern '$pattern'). It cannot be granted; do not retry it or work around it, tell the user instead."
      )

    val missing = lines.filterNot(policy.commandAllowed(scope, _)).distinct
    if missing.nonEmpty then
      val listed = missing.map(line => s"'$line'").mkString(", ")
      val target =
        if pipeline.stages.lengthIs > 1 then s"stage${if missing.lengthIs > 1 then "s" else ""} $listed of the pipeline"
        else if missing.lengthIs == 1 then s"command $listed"
        else s"commands $listed"
      val verb = if missing.lengthIs == 1 then "matches" else "match"
      val variables =
        if assignments.isEmpty then ""
        else " A command run with withEnv needs its own permission and one `NAME=value command` for each variable."
      val patterns = missing.map(ScalaSource.stringLiteral).mkString(", ")
      throw SecurityException(
        s"Access denied: $target $verb no permitted pattern.$variables Use requestExec(Set($patterns), reason) { ... } to ask the user."
      )

  private def commandDirectory(path: String, fs: FileSystem): Path =
    val dir = canonical(path)
    requireReadable(scopeOf(fs), dir, "running a command in", "a working directory outside it")
    dir

  private def inputRedirect(path: String, fs: FileSystem): Path =
    val input = canonical(path)
    requireReadable(scopeOf(fs), input, "feeding a command from", "an unclassified file")
    input

  private def outputRedirect(path: String, fs: FileSystem): Path =
    val target = canonical(path)
    requireWritable(scopeOf(fs), target, "redirecting a command's output to", "an unclassified file")
    ensureParent(target)
    target

  private def processBuilders(
    pipeline: CommandLine.Pipeline,
    dir: Path,
    env: Map[String, String]
  ): List[ProcessBuilder] =
    val keys = keyVariables()
    pipeline.stages.map: stage =>
      val argv = WindowsExecutable.resolve(stage.argv, dir)
      val builder = ProcessBuilder(argv.asJava).directory(dir.toFile).nn
      val environment = builder.environment().nn
      // Windows launchers may carry the original command line (including a prompt)
      // in these variables, and the key variables hold ATC's credentials. They belong
      // to ATC, not to commands the agent starts, which could print them. Windows names
      // ignore case, but this map's keys keep the system's spelling (`Path`, not `PATH`).
      environment.keySet().nn.removeIf: name =>
        LauncherEnvironment.isInternal(name) ||
          keys.exists(key => if Platform.isWindows then key.equalsIgnoreCase(name) else key == name)
      // The agent's own variables come last: a value it sets reveals nothing of what it
      // replaces. On Windows an inherited `Path` must go before `PATH` is put beside it.
      if Platform.isWindows then environment.keySet().nn.removeIf(name => env.keys.exists(_.equalsIgnoreCase(name)))
      environment.putAll(env.asJava)
      HostProcesses.refuseImplicitBareRepositories(environment)
      if stage.mergeErr then builder.redirectErrorStream(true)
      builder

  private def prepare(command: String, args: Seq[String], options: ExecOptions)(using
    ex: Exec,
    fs: FileSystem
  ): Prepared =
    val pipeline = withArgs(command, args)
    if options.stdin.nonEmpty && pipeline.stdinFile.isDefined then
      throw IllegalArgumentException(
        "exec: both ExecOptions(stdin = ...) and '< file' would feed the command; use one of them"
      )
    val env = envOf(ex)
    val assignments = env.toList.sorted.map(CommandLine.assignment)
    authorizeCommands(pipeline, scopeOf(ex), assignments)

    val dir = commandDirectory(options.workingDir, fs)
    val stdinFile = pipeline.stdinFile.map(inputRedirect(_, fs))
    val stdoutFile = pipeline.stdoutFile.map(outputRedirect(_, fs))
    val pbs = processBuilders(pipeline, dir, env)
    stdinFile.foreach(path => pbs.head.redirectInput(path.toFile))
    stdoutFile.foreach: path =>
      val file = path.toFile
      pbs.last.redirectOutput(if pipeline.append then Redirect.appendTo(file) else Redirect.to(file))
    // The user sees the variables once, in front; they apply to every stage.
    Prepared(pbs, pipeline.stages.map(_.line), (assignments :+ pipeline.line).mkString(" "))

  def exec(command: String, args: Seq[String], options: ExecOptions)(using ex: Exec, fs: FileSystem): ProcessResult =
    if options.timeoutMs <= 0 then
      throw IllegalArgumentException(s"exec: timeoutMs must be positive (got ${options.timeoutMs})")
    val prepared = prepare(command, args, options)
    val port = output
    val live = new Processes.LiveOutput:
      def begin(): Unit = port.commandRunning(prepared.line)
      def output(text: String): Unit = port.commandOutput(text)
    output.whileCommandRuns(
      Processes.run(
        prepared.pbs,
        prepared.stageLines,
        prepared.line,
        options.timeoutMs,
        Some(live),
        options.stdin
      )
    )

  // The registry is per host; ids are never reused within a session.
  private val spawned = mutable.LinkedHashMap[Int, ProcessImpl]()
  private var nextProcessId = 0

  def spawn(command: String)(using Exec, FileSystem): Process = spawn(command, ExecOptions())

  def spawn(command: String, options: ExecOptions)(using ex: Exec, fs: FileSystem): Process =
    val prepared = prepare(command, Nil, options)
    spawned.synchronized:
      reapProcesses()
      if spawned.size >= Host.MaxProcesses then
        throw IllegalStateException(
          s"spawn: ${Host.MaxProcesses} processes are already running (${spawned.keys.map(id => s"p$id").mkString(", ")}); kill() one first"
        )
      nextProcessId += 1
      val id = nextProcessId
      val managed = Processes.ManagedProcess.start(
        prepared.pbs,
        prepared.stageLines,
        prepared.line,
        options.stdin,
        closeStdinAfter = false,
        live = None,
        keepHead = false,
        onExit = code => output.processExited(id, code),
      )
      val handle = ProcessImpl(id, managed, output, scopeOf(ex), policy)
      spawned(id) = handle
      output.processStarted(id, prepared.line)
      handle

  /** Return the live processes visible from the caller's scope. */
  def runningProcesses(using ex: Exec): List[Process] = spawned.synchronized:
    reapProcesses()
    val caller = scopeOf(ex)
    spawned.values.toList.filter(process => policy.scopeVisibleFrom(caller, process.scope))

  private def reapProcesses(): Unit = spawned.filterInPlace((_, process) => process.managed.isAlive)

  /** Kill the processes owned by a closing `request*` scope. */
  private[host] def killProcessesInScope(id: ScopeId): Unit = spawned.synchronized:
    spawned.values.toList.filter(_.scope == id).foreach: process =>
      try process.managed.kill()
      catch case NonFatal(_) => ()
    reapProcesses()

  /** Kill every spawned process at session end or for `/kill all`. */
  private[atc] def killProcesses(): Unit = spawned.synchronized:
    spawned.values.foreach(_.managed.kill())
    spawned.clear()

  /** `/kill`: `p3`, `3`, or `all`. A user-facing report: `Right` when processes were killed,
    * `Left` when there was none to kill. */
  private[atc] def killProcess(ref: String): Either[String, String] = spawned.synchronized:
    reapProcesses()
    ref.trim.toLowerCase(Locale.ROOT) match
      // Stopping everything takes the word: a bare `/kill`, which the command list also
      // fills in for `/k`, must not end a server the user meant to keep.
      case "" => Left("/kill p<number> stops one process (see /ps); /kill all stops every one")
      case "all" =>
        val count = spawned.size
        killProcesses()
        if count == 0 then Left("no process is running")
        else Right(s"killed $count process${if count == 1 then "" else "es"}")
      case value =>
        value.stripPrefix("p").toIntOption.flatMap(spawned.get) match
          case Some(process) =>
            process.managed.kill()
            spawned.remove(process.id)
            Right(s"killed p${process.id} (${process.commandLine})")
          case None => Left(s"no running process '$ref' (see /ps)")

  /** `/ps`: one line per live process. */
  private[atc] def processSummary: String = spawned.synchronized:
    reapProcesses()
    if spawned.isEmpty then "no process is running"
    else spawned.values.map(process => s"p${process.id}  ${process.commandLine}").mkString("\n")

  def execOutput(command: String)(using Exec, FileSystem): String = execOutput(command, Nil, ExecOptions())

  def execOutput(command: String, args: Seq[String])(using Exec, FileSystem): String =
    execOutput(command, args, ExecOptions())

  def execOutput(command: String, args: Seq[String], options: ExecOptions)(using Exec, FileSystem): String =
    val result = exec(command, args, options)
    if result.exitCode != 0 then
      val error = result.stderr.trim
      val tail = if error.isEmpty then "" else s"; stderr: ${error.takeRight(Host.ExecErrorTailChars)}"
      throw RuntimeException(
        s"'${withArgs(command, args).line}' exited with ${result.exitCode}$tail (use exec(...) to inspect a failure)"
      )
    result.stdout

private[host] object HostProcesses:
  /** Git takes a directory laid out as a bare repository (`HEAD`, `objects`, `refs`) for one
    * and runs the programs its `config` names (`textconv`, `diff.external`), and the agent may
    * write such a directory wherever its rules let it write. `safe.bareRepository=explicit`
    * (git 2.38+) refuses a bare repository found that way. It is appended after the numbered
    * configuration parameters the command inherits or the agent sets. Git reads
    * `GIT_CONFIG_PARAMETERS` after those, and the last value wins, so the setting goes at the
    * end of that variable too when it is set. A `-c` on git's own command line still comes
    * later; the command pattern has to permit it. */
  def refuseImplicitBareRepositories(environment: java.util.Map[String, String]): Unit =
    val count = Option(environment.get("GIT_CONFIG_COUNT")).flatMap(_.trim.toIntOption).filter(_ >= 0).getOrElse(0)
    environment.put(s"GIT_CONFIG_KEY_$count", "safe.bareRepository")
    environment.put(s"GIT_CONFIG_VALUE_$count", "explicit")
    environment.put("GIT_CONFIG_COUNT", (count + 1).toString)
    Option(environment.get("GIT_CONFIG_PARAMETERS")).filter(_.trim.nonEmpty).foreach: parameters =>
      environment.put("GIT_CONFIG_PARAMETERS", parameters + " 'safe.bareRepository=explicit'")
