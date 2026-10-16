package atc

import atc.ui.Ansi

/** `ATC_DEBUG=1`: stack traces and terminal/stream diagnostics on stderr. */
object Debug:
  /** Include nested transport failures without requiring a full stack trace. */
  def describe(error: Throwable): String =
    causes(error).map(c => c.getClass.getSimpleName + Option(c.getMessage).filter(_.nonEmpty).fold("")(": " + _))
      .mkString("\nCaused by: ")

  /** `error` and its causes, at most eight and each once, so a cyclic chain ends. */
  def causes(error: Throwable): Iterator[Throwable] =
    Iterator.iterate[Throwable | Null](error)(_.nn.getCause).takeWhile(_ != null).map(_.nn).take(8).distinct

  /** The message of `error`, or its class name when it has none: for one-line reports. */
  def message(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getSimpleName)

  val enabled: Boolean = ProcessEnvironment.contains("ATC_DEBUG")
  /** The process's own stderr: the REPL redirects `System.err` while it evaluates, and
    * a line logged then from another thread would land in its output, where a set-up
    * round reads "error" in it as a compile failure. */
  private val stderr = java.io.PrintStream(java.io.FileOutputStream(java.io.FileDescriptor.err), true)
  inline def log(inline message: String): Unit = if enabled then stderr.println(line(message))
  def trace(e: Throwable): Unit = if enabled then
    val text = java.io.StringWriter()
    e.printStackTrace(java.io.PrintWriter(text))
    stderr.print(Ansi.sanitize(text.toString))

  /** A logged line without terminal controls: messages carry provider and exception
    * text, and stderr is usually the terminal. */
  private[atc] def line(message: String): String = "[atc] " + Ansi.sanitize(message)
