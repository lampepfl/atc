package atc.ui

import atc.{Debug, Threads}
import atc.platform.Platform

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{Executor, LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

/** The name of the tmux window atc runs in, which tmux shows as the window's tab. tmux
  * names a window after the command of its pane (`automatic-rename`), which for atc is
  * `java`, and a title set with OSC 0 only becomes the pane's title, which the default
  * status line does not show. So atc names the window itself (`rename-window`), as long as
  * tmux was the one naming it: a window the user named keeps its name. Naming a window
  * turns `automatic-rename` off for it, and `restore` turns it back on.
  *
  * `run` runs a tmux command and gives its output, `None` when it failed. The commands go
  * to `executor` one after another, so the caller, which holds the screen's lock, never
  * waits for tmux. */
private[ui] final class TmuxWindow(pane: String, run: List[String] => Option[String], executor: Executor):
  /** The window's own `automatic-rename` before atc named it: empty when it followed the
    * session's setting. `None` when tmux does not name this window, or cannot be asked. */
  private lazy val before: Option[String] =
    for
      naming <- run(List("display-message", "-p", "-t", pane, "#{automatic-rename}"))
      if naming.trim == "1"
      own <- run(List("show-options", "-w", "-q", "-v", "-t", pane, "automatic-rename"))
    yield own.trim
  /** Whether atc named the window, and so has to hand the naming back. Read and written by the commands. */
  private var named = false
  @volatile private var restored = false

  /** Name the window `name`, unless the user named it. */
  def rename(name: String): Unit = executor.execute: () =>
    // `--`: a name that starts with `-` is not an option.
    if !restored && before.isDefined then named = run(List("rename-window", "-t", pane, "--", name)).isDefined || named

  /** Let tmux name the window again, as it did before; later names are ignored. */
  def restore(): Unit =
    restored = true
    executor.execute: () =>
      if named then
        named = false
        // Unset, the window follows the session's setting again, which was on.
        val handBack = if before.contains("") then List("-u", "automatic-rename") else List("automatic-rename", "on")
        run(List("set-option", "-w", "-t", pane) ++ handBack)
        ()

private[ui] object TmuxWindow:
  /** How long a tmux command may take: a server that does not answer is given up on. */
  private val TimeoutMillis = 2000L

  /** The window of the pane this process runs in, from the variables tmux sets; `None`
    * outside tmux. It hands the naming back when the JVM exits, however it does, since a
    * window left with atc's name would keep it. */
  def current(env: String => Option[String]): Option[TmuxWindow] =
    for
      _ <- env("TMUX").filter(_.nonEmpty)
      pane <- env("TMUX_PANE").filter(_.nonEmpty)
    yield
      // One thread, and a name that comes after the hook below shut it down is dropped.
      val commands = ThreadPoolExecutor(
        1,
        1,
        0L,
        TimeUnit.MILLISECONDS,
        LinkedBlockingQueue[Runnable](),
        Threads.daemon("atc-tmux")(_),
        ThreadPoolExecutor.DiscardPolicy(),
      )
      val window = TmuxWindow(pane, tmux, commands)
      Runtime.getRuntime.nn.addShutdownHook(Thread: () =>
        window.restore()
        commands.shutdown()
        commands.awaitTermination(TimeoutMillis, TimeUnit.MILLISECONDS)
        ())
      window

  /** Run `tmux` with `arguments`, which reach it as they are, without a shell. Its output
    * is a word or nothing, so it is read once the command has ended. */
  private def tmux(arguments: List[String]): Option[String] =
    try
      val process = ProcessBuilder(("tmux" :: arguments)*)
        .redirectInput(ProcessBuilder.Redirect.from(Platform.nullDevice))
        .redirectError(ProcessBuilder.Redirect.DISCARD)
        .start()
      if process.waitFor(TimeoutMillis, TimeUnit.MILLISECONDS) then
        Option.when(process.exitValue == 0)(String(process.getInputStream.nn.readAllBytes(), UTF_8))
      else
        process.destroyForcibly()
        None
    catch
      case e: Exception =>
        Debug.log(s"tmux ${arguments.headOption.getOrElse("")} failed: $e")
        None
