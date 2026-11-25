package atc.sandbox

import atc.Threads
import atc.host.{ClassifiedImpl, Host, MonitorRepl}
import atc.perms.Mode
import atc.platform.Platform

import dotty.tools.repl.*
import dotty.tools.dotc.reporting.Diagnostic

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.util.Locale
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.util.control.NonFatal

object ReplSession:
  /** Drop the trace the REPL prints when the stop flag aborts the snippet (`ThreadDeath`
    * from the stop check, or the `ExceptionInInitializerError` it surfaces as in a
    * top-level definition): it is the last thing printed and says nothing to the model.
    * Only a trace that ends the output goes: the snippet may print such a line itself. */
  private[atc] def withoutStopTrace(output: String): String =
    val lines = output.linesIterator.toVector
    val start = lines.lastIndexWhere: line =>
      line.startsWith("java.lang.ThreadDeath") || line.startsWith("java.lang.ExceptionInInitializerError")
    def traceLine(line: String) =
      val text = line.trim
      text.isEmpty || text.startsWith("at ") || text.startsWith("Caused by:") || text.startsWith("...")
    if start < 0 || !lines.drop(start + 1).forall(traceLine) then output
    else lines.take(start).mkString("\n").stripTrailing()

  val MaxOutputBytes: Int = 4 * 1024 * 1024
  val TruncationMarker: String = "\n... [output truncated: exceeded 4 MiB capture limit]"

  /** Bounded capture buffer: retains at most `limit` bytes. It drops what a thread writes
    * while it runs code on classified data ([[ClassifiedImpl.running]]): the capture is
    * model-visible, and `System.out` and `System.err` point here during an evaluation. It
    * drops what a monitor's run writes too, which is no part of the evaluation. */
  final class BoundedOutputStream(limit: Int) extends OutputStream:
    private val buffer = ByteArrayOutputStream(math.min(limit, 8192))
    @volatile var truncated: Boolean = false

    override def write(b: Int): Unit =
      if ClassifiedImpl.running || Host.inMonitor then ()
      else if buffer.size() < limit then buffer.write(b)
      else truncated = true

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      val room = limit - buffer.size()
      if ClassifiedImpl.running || Host.inMonitor then ()
      else if room <= 0 then truncated = true
      else
        if len > room then truncated = true
        buffer.write(b, off, math.min(len, room))

    def resetCapture(): Unit =
      buffer.reset()
      truncated = false

    def capturedString: String = buffer.toString(StandardCharsets.UTF_8)

  private final class OpenReplDriver(
    settings: Array[String],
    out: PrintStream,
    cl: Option[ClassLoader],
    maxEchoChars: Int
  ) extends ReplDriver(settings, out, cl):
    // Replace the stock renderer before the first evaluation (it creates the REPL class loader lazily).
    private val capped = CappedRendering(cl, maxEchoChars)
    rendering = capped
    def runParseResult(res: ParseResult)(using State): State = runBody(interpret(res))
    /** Whether an evaluation threw an uncaught exception since the last reset. */
    def evaluationThrew: Boolean = capped.threw
    def resetEvaluationThrew(): Unit = capped.resetThrew()
    /** The loader of the REPL-defined classes (null before the first evaluation). */
    def replClassLoader: ClassLoader | Null = rendering.myClassLoader
    /** Raise or clear the stop flag checked by the instrumented REPL classes (loop back edges and
      * method entries throw `ThreadDeath` while it is set). */
    def setStopFlag(stop: Boolean): Unit =
      val loader = replClassLoader
      if loader != null then ReplBytecodeInstrumentation.setStopFlag(loader, stop)

  private def compilerArgs(classpath: String): Array[String] = Array(
    "-classpath",
    classpath,
    "-color:never",
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Yexplicit-nulls",
    "-Wsafe-init",
    "-language:experimental.captureChecking",
    "-language:experimental.modularity",
    // Interrupt instrumentation makes loops in REPL-defined classes stoppable
    // (`local` would instrument nothing at all). Library classes are protected
    // from being re-defined by `Sandbox.SandboxLoader`, which hides their
    // bytecode resources from the REPL loader.
    "-Xrepl-interrupt-instrumentation:true",
    // Every top-level `val` bound to a Java-returning call (`s.replace(...)`)
    // would warn that it "exposes a flexible type": noise for agent code.
    "-Wconf:msg=exposes a flexible type:s",
    // A `return` inside a lambda is a deprecated non-local return: it throws a control
    // throwable through the code that called the lambda, which is easy to misuse.
    "-Wconf:msg=Non local returns are no longer supported:e",
  )

  /** Definitions in scope before any agent code runs, for `mode`.
    *
    * `object api` holds the host privately; the preamble publishes as implicit
    * values only the root view and the derived capabilities of the mode:
    *
    *  - full: `io: IOCap^` (the root itself) and `fs`/`ex`/`net` derived from it;
    *  - local: `io: IOCap^` (the root itself) and `fs`/`ex` derived from it,
    *    with no network capability;
    *  - read-only: `io: IOCap` and the read-only `fs` derived from it.
    *
    * The root records the capture hierarchy; it is not itself a factory. The
    * `Runtime` derivations create the capabilities, and agent code cannot call them, so
    * local mode cannot produce the omitted `net` from its full `io`.
    *
    * `atc.lib.Runtime.current`/`.rootIO` are `@rejectSafe`: the preamble is
    * compiled before the safe-mode import, agent code after it, so agent code
    * cannot name them (nor `api.host`/`api.root`, which are private).
    *
    * They are strict `implicit val`s, not givens. A given is a lazy val, and the
    * checker charges a closure that uses one with the whole line wrapper holding
    * it, which no capture set names: a callback typed `->{io}` could then use
    * none of them. A strict val is the path `wrapper.fs`, charged as `fs`, whose
    * type says what it captures. That relies on `Cap`'s update member: without
    * one the checker charged every use of `user` or `fs` as read-only, which a
    * pure `Classified.map` admits.
    *
    * The chunks are loaded as separate REPL rounds (`init`): the base
    * (`object api` and imports) first, then each capability in its own round, a
    * field of its own line wrapper. */
  def preambleChunks(mode: Mode): List[String] =
    val base =
      """|import language.experimental.captureChecking
         |import atc.lib.*
         |import caps.*
         |@assumeSafe object api:
         |  private val host: Interface = atc.lib.Runtime.current
         |  export host.*
         |import api.*""".stripMargin
    val givens = mode match
      case Mode.Full =>
        List(
          "@assumeSafe implicit val io: IOCap^ = atc.lib.Runtime.rootIO",
          "@assumeSafe implicit val user: UserIO^ = atc.lib.Runtime.rootUser",
          "@assumeSafe implicit val fs: FileSystem^{io} = atc.lib.Runtime.fileSystem",
          "@assumeSafe implicit val ex: Exec^{io} = atc.lib.Runtime.processes",
          "@assumeSafe implicit val net: Network^{io} = atc.lib.Runtime.network",
        )
      case Mode.Local =>
        List(
          "@assumeSafe implicit val io: IOCap^ = atc.lib.Runtime.rootIO",
          "@assumeSafe implicit val user: UserIO^ = atc.lib.Runtime.rootUser",
          "@assumeSafe implicit val fs: FileSystem^{io} = atc.lib.Runtime.fileSystem",
          "@assumeSafe implicit val ex: Exec^{io} = atc.lib.Runtime.processes",
        )
      case Mode.ReadOnly =>
        List(
          "@assumeSafe implicit val io: IOCap = atc.lib.Runtime.rootIO",
          "@assumeSafe implicit val user: UserIO^ = atc.lib.Runtime.rootUser",
          "@assumeSafe implicit val fs: FileSystem^{io.rd} = atc.lib.Runtime.readOnlyFileSystem",
        )
    base :: givens

  private val SafeModeImport = "import language.experimental.safe"

  /** System.out/err are swapped around each evaluation to capture compiler
    * diagnostics that bypass the driver's stream; that is process-global, so
    * evaluations are serialized. A `ReentrantLock` (rather than `synchronized`)
    * lets a run give up when a previous evaluation was interrupted but its
    * thread never died: that thread holds the stream forever, and waiting on
    * it with `synchronized` would block every subsequent evaluation. */
  private val outputLock = ReentrantLock()
  private val OutputLockWaitMs = 10000L

  /** The process's `System.err` without what a monitor's run writes. A monitor runs agent
    * code outside evaluations too, when `System.err` is the terminal, and `printStackTrace`
    * writes there without a capability (agent code cannot name `System.out`). It encodes as
    * the JDK encodes `System.err`. */
  private final class MonitorSilenced(stream: PrintStream)
      extends PrintStream(
        new java.io.FilterOutputStream(stream):
          override def write(b: Int): Unit = if !Host.inMonitor then stream.write(b)
          override def write(b: Array[Byte], off: Int, len: Int): Unit =
            if !Host.inMonitor then stream.write(b, off, len)
        ,
        true,
        List("stderr.encoding", "sun.stderr.encoding")
          .flatMap(property => Option(System.getProperty(property)))
          .flatMap(name => scala.util.Try(java.nio.charset.Charset.forName(name).nn).toOption)
          .headOption
          .getOrElse(java.nio.charset.Charset.defaultCharset()),
      )

  /** Wrap `System.err` in [[MonitorSilenced]], once. Under the evaluation lock it is the
    * process's own, not an evaluation's capture. */
  private def silenceMonitors(): Unit =
    if outputLock.tryLock(OutputLockWaitMs, TimeUnit.MILLISECONDS) then
      try
        System.err match
          case _: MonitorSilenced => ()
          case err => System.setErr(MonitorSilenced(err.nn))
      finally outputLock.unlock()

  /** The lock is JVM-wide (so is `System.out`): a new session cannot recover
    * from a stuck evaluation either, only a restart of the process can. */
  private val StuckEvaluationMessage =
    "A previous evaluation is still running (it could not be stopped) and holds the output stream, " +
      "so nothing can be evaluated until atc is restarted; tell the user."

  /** What one evaluation produced, before it is adopted into the session:
    * the new state, the captured output, the message of what escaped the driver (`thrown`),
    * and whether agent code threw an exception the REPL rendered (`failed`). */
  private final case class Evaluated(state: State, output: String, thrown: Option[String], failed: Boolean)

  private val InterruptedMessage = "Execution interrupted by the user (completed effects are not rolled back)"
  private val NoResultMessage = "Execution failed (no result; possible fatal error)"
  private val TooDeeplyNestedMessage =
    "The code nests too deeply for the compiler to parse (stack overflow); split the expression into smaller definitions."

/** One persistent REPL with its own sandbox class loader and host. */
final class ReplSession(config: SandboxConfig, host: Host, preambleOverride: Option[String] = None)
    extends MonitorRepl:
  import ReplSession.*

  private val outputCapture = BoundedOutputStream(MaxOutputBytes)
  /** Where agent-visible output goes: REPL results, diagnostics, and (via the
    * host's `print`) the agent's own `println` calls. */
  val printStream: PrintStream = PrintStream(outputCapture, true, StandardCharsets.UTF_8)
  val clock: ExecutionClock = ExecutionClock()

  private val classpath = Sandbox.libraryClasspath.map(_.toString).mkString(Platform.pathListSeparator)
  private val driver =
    OpenReplDriver(compilerArgs(classpath), printStream, Some(Sandbox.newLoader()), config.maxEchoChars)
  private var state: State = driver.initialState
  @volatile private var evalThread: Thread | Null = null
  /** Set while a stop (interrupt or timeout) is in flight for the current run. */
  @volatile private var stopRequested: Boolean = false
  @volatile private var closed: Boolean = false

  /** Load the preamble; errors here are programmer bugs and are thrown. Each
    * chunk is a separate REPL round so that every capability lands in its own
    * line-wrapper (see `preambleChunks`); a `preambleOverride` runs as one round.
    * Each round installs this session's host under the evaluation lock; installing it
    * before would replace the host of a session that holds the lock. */
  def init(): this.type =
    silenceMonitors()
    val chunks = preambleOverride.map(List(_)).getOrElse(preambleChunks(config.mode))
    chunks.foreach(setUp("Sandbox preamble", _))
    if config.safeMode then setUp("Safe mode import", SafeModeImport)
    this

  /** Evaluate one set-up round, failing loudly: nothing the agent writes has run yet. */
  private def setUp(what: String, code: String): Unit =
    val (out, thrown) = withOutputCapture() {
      // Runtime's bootstrap slot is process-global. The same JVM may host more
      // than one session, so select this session's host while holding the same
      // process-wide lock that serializes evaluation and lazy wrapper loading.
      Sandbox.installHost(host)
      host.repl = this
      state = driver.run(code)(using state)
    }
    thrown.foreach(throw _)
    if out.toLowerCase(Locale.ROOT).contains("error") then
      throw IllegalStateException(s"$what failed to compile:\n$out")

  /** End the session: stop a running evaluation (best effort) and refuse
    * further runs. The compiler and its class loader are not released here:
    * they are collected once nothing refers to the session any more. */
  def close(): Unit =
    closed = true
    interrupt()

  /** Interrupt a running evaluation (best effort): raise the REPL stop flag
    * for loops in agent code and interrupt the thread for blocking calls. The
    * request is recorded even when no evaluation thread is visible yet (an
    * interrupt landing just before `evalThread` is set must not be lost). */
  def interrupt(): Unit =
    stopRequested = true
    driver.setStopFlag(true)
    val t = evalThread
    if t != null then t.interrupt()

  /** A monitor runs this REPL's code outside evaluations, under the same stop flag. Holding
    * the evaluation lock shows that no evaluation runs, so a raised flag is one an interrupt
    * left behind (Ctrl-C raises it even between runs) and is lowered. While an evaluation
    * runs, the flag is that evaluation's: a run it stops ends with it. */
  def readyForMonitor(): Boolean =
    if !closed && outputLock.tryLock() then
      try if !closed then driver.setStopFlag(false)
      finally outputLock.unlock()
    !closed

  def run(code: String): ExecutionResult = run(code, () => false)

  /** [[run]] with the `texts` the agent's call sent, which its code reads through `texts`. */
  def run(code: String, texts: Map[String, String], cancelled: () => Boolean): ExecutionResult =
    host.withTexts(texts)(run(code, cancelled))

  /** [[run]] for a caller with a stop flag of its own, `cancelled`: raised before this call,
    * when an `interrupt()` it caused may already have been cleared, it keeps the code from
    * running. */
  def run(code: String, cancelled: () => Boolean): ExecutionResult =
    clock.reset() // per run, whichever way it ends (callers read `clock.paused` afterwards)
    // Cleared before validation, so that a stop arriving while the code is checked or parsed
    // still prevents it from running; one that arrived earlier shows in `cancelled`.
    stopRequested = false
    if cancelled() then stopRequested = true
    if closed then ExecutionResult.failed("The sandbox session is closed; start a new one.")
    else
      // The validator's scan and the parser run here, on the caller's thread, and both recurse
      // with the nesting of the code. Code nested thousands deep must fail like any other
      // code the compiler cannot take, not end the program with a stack overflow.
      val parsed =
        try
          CodeValidator.validate(code, config.safeMode) match
            case Nil => Right(ParseResult(code.stripTrailing() + "\n")(using state))
            case violations => Left(ExecutionResult.failed(CodeValidator.formatErrors(violations)))
        catch case _: StackOverflowError => Left(ExecutionResult.failed(TooDeeplyNestedMessage))
      parsed.fold(identity, runParsed)

  private def runParsed(result: ParseResult): ExecutionResult =
    result match
      case p: Parsed => dispatch(p)
      case cmd @ (_: TypeOf | _: DocOf | Imports) => dispatch(cmd)
      case _: Command => ExecutionResult.failed("Only :type, :doc, and :imports REPL commands are allowed.")
      case Newline => ExecutionResult(true, "")
      case SyntaxErrors(_, errors, _) =>
        ExecutionResult.failed("Syntax error:\n" + formatDiagnostics(errors))
      case other => ExecutionResult.failed(s"Unexpected parse result: $other")

  /** Evaluate on a worker thread, also without a time limit, so that interrupts, fatal
    * errors and stop signals take one path. */
  private def dispatch(res: ParseResult): ExecutionResult =
    dispatchWithTimeout(res, config.executionTimeoutMs.getOrElse(Long.MaxValue))

  /** `started` is released once the output stream is ours (or we gave up waiting for it). */
  private def evaluate(res: ParseResult, started: CountDownLatch = CountDownLatch(0)): Evaluated =
    var newState = state
    val (output, thrown) = withOutputCapture(onEnter = started.countDown()) {
      // An interrupt may have landed while we waited for the output lock. `run`
      // cleared `stopRequested` at the top, so it is true only if `interrupt()`
      // fired during this run. Then the cancelled code must not execute, and the
      // stop flag it raised must stay raised; `adopt` reports the abort. The flag
      // a previous evaluation may have left is cleared before the check, so that
      // an interrupt between the two still finds its flag raised.
      driver.setStopFlag(false)
      if stopRequested then driver.setStopFlag(true)
      else
        // The preamble's `object api` can be initialized lazily on the first agent
        // line, long after `init`. Re-select the owning host inside the global
        // evaluation lock so another session cannot supply its capabilities.
        Sandbox.installHost(host)
        host.repl = this // a monitor this code adds runs code of this REPL
        driver.resetEvaluationThrew()
        newState = driver.runParseResult(res)(using state)
    }
    // What escapes the driver may be the agent's own exception, thrown by a `toString` the
    // REPL called to show a value. Its message is agent code and is read here, on the
    // evaluation thread and within its limits, not by the caller.
    val problem = thrown.map: e =>
      try Option(e.getMessage).getOrElse(e.toString)
      catch case NonFatal(_) => e.getClass.getName.nn
    Evaluated(newState, output, problem, driver.evaluationThrew)

  /** Take over the new state and turn the evaluation into the agent-visible result. */
  private def adopt(res: ParseResult, evaluated: Evaluated): ExecutionResult =
    val Evaluated(newState, output, thrown, failed) = evaluated
    val previousIndex = state.objectIndex
    state = newState
    if stopRequested then
      // The evaluation was interrupted: its wrapper class may be half-initialized
      // (`ThreadDeath` from the stop check surfaces as an `ExceptionInInitializerError`
      // the REPL renders as normal output). Skip that wrapper index so the next
      // line does not collide with the invalid class, and report the abort.
      skipInvalidWrapper(previousIndex)
      ExecutionResult.failed(InterruptedMessage)
    else
      thrown match
        case Some(problem) =>
          // The driver did not return a state, but it had compiled and loaded this line's
          // wrapper: the next line must not take the same index and find that class.
          skipInvalidWrapper(previousIndex)
          ExecutionResult(false, output, Some(problem))
        case None =>
          val compileFailed = res match
            case p: Parsed => p.reporter.hasErrors
            case _ => false
          ExecutionResult(!compileFailed && !failed, output)

  /** Exclude the stopped wrapper while preserving every earlier definition. The driver may
    * already have advanced its index; advancing again would leave an unregistered valid index. */
  private def skipInvalidWrapper(previousIndex: Int): Unit =
    val stoppedIndex = state.objectIndex.max(previousIndex + 1)
    state = state.copy(
      objectIndex = stoppedIndex,
      invalidObjectIndexes = state.invalidObjectIndexes + stoppedIndex,
    )

  private def dispatchWithTimeout(res: ParseResult, limitMs: Long): ExecutionResult =
    val previousIndex = state.objectIndex
    val resultRef = AtomicReference[Evaluated]()
    val started = CountDownLatch(1)
    val worker = Threads.daemon("atc-repl-eval")(() => resultRef.set(evaluate(res, started)))
    evalThread = worker
    worker.start()
    try
      // The timeout is execution time: it starts once the worker owns the output
      // stream (a stuck previous evaluation may hold it for up to OutputLockWaitMs,
      // after which the worker gives up and reports that instead), and does not
      // count time the host spends waiting for the user.
      while worker.isAlive && !started.await(50, TimeUnit.MILLISECONDS) do ()
      val start = System.nanoTime()
      var remaining = limitMs
      while worker.isAlive && remaining > 0 && !stopRequested do
        worker.join(math.min(remaining, 200L))
        // Paused time can exceed the elapsed time (a stuck earlier evaluation pauses the same
        // clock), and without a limit, Long.MaxValue minus a negative span would overflow.
        remaining = limitMs - (System.nanoTime() - start - clock.paused).max(0L) / 1_000_000L
      // A stop from the user has interrupted the worker already. It gets the grace a timeout
      // gives, then the call returns even when the worker is in host code that ignores it.
      if stopRequested then worker.join(2000)
      if worker.isAlive then
        val timedOut = !stopRequested
        if timedOut then
          interrupt()
          worker.join(2000)
        val evaluated = resultRef.get()
        if evaluated != null then state = evaluated.state
        skipInvalidWrapper(previousIndex)
        // What the snippet printed before the limit is still useful to the model: the
        // worker's own output when it stopped, else a snapshot of the capture it is
        // still writing to.
        val output =
          if !timedOut then "" // the turn ends anyway, as after an interrupt that stopped
          else ReplSession.withoutStopTrace(if evaluated != null then evaluated.output else capturedSoFar)
        val note = if worker.isAlive then "; the evaluation could not be stopped and is still running" else ""
        val problem =
          if timedOut then s"Execution timed out after ${limitMs}ms (completed effects are not rolled back)"
          else InterruptedMessage
        ExecutionResult(false, output, Some(problem + note))
      else
        resultRef.get() match
          case null =>
            // The worker died without a result; its wrapper may be half-initialized.
            skipInvalidWrapper(previousIndex)
            ExecutionResult.failed(NoResultMessage)
          case evaluated => adopt(res, evaluated)
    finally evalThread = null

  /** Run `run` with System.out/err captured; `onEnter` fires as soon as the
    * wait for the (process-wide) output stream is over, either way. */
  private def withOutputCapture(onEnter: => Unit = ())(run: => Unit): (String, Option[Throwable]) =
    val acquired =
      try outputLock.tryLock(OutputLockWaitMs, TimeUnit.MILLISECONDS)
      catch
        // Interrupted while waiting (a user interrupt can land here). The lock may
        // be free, so retry once, uninterruptibly, before reporting a blocked evaluation.
        case _: InterruptedException => outputLock.tryLock()
    onEnter
    if !acquired then ("", Some(RuntimeException(StuckEvaluationMessage)))
    else
      try
        outputCapture.resetCapture()
        val oldOut = System.out
        val oldErr = System.err
        System.setOut(printStream)
        System.setErr(printStream)
        val thrown =
          try
            run
            None
          catch case NonFatal(e) => Some(e)
          finally
            System.setOut(oldOut)
            System.setErr(oldErr)
            printStream.flush()
        (capturedSoFar, thrown)
      finally outputLock.unlock()

  /** The output captured so far. Only trailing whitespace is dropped: the UI removes the
    * agent's own prints (already shown live) from this text and needs an exact match. */
  private def capturedSoFar: String =
    val captured = outputCapture.capturedString.stripTrailing()
    if outputCapture.truncated then captured + TruncationMarker else captured

  private def formatDiagnostics(diags: List[Diagnostic]): String =
    diags.map: d =>
      val pos = d.pos
      if pos != null && pos.exists then s"Line ${pos.line + 1}: ${d.message}" else d.message
    .mkString("\n")
