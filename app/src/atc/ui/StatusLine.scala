package atc.ui

import atc.agent.TurnOutcome

import org.jline.terminal.{Size, Terminal}
import org.jline.utils.{AttributedString, Status}

import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import scala.jdk.CollectionConverters.*

/** The footer (JLine `Status`), the window title and the program status. The footer shows
  * what the turn is doing, an unsent correction, or the keys a pop-up takes; between turns
  * it shows the mode, model and directory. The title is `atc · <directory>`, marked while a
  * turn runs (`●`) or a pop-up waits during one (`?`), so a tab that needs the user stands
  * out. The terminal's own title is saved first (xterm's title stack) and restored by
  * `close`. The program status (OSC 7501) says the same to terminals that track it:
  * `working`, `blocked` on a pop-up, then `done`, `error` or `idle` when the turn ends.
  *
  * Updates run with the screen's lock held and redraw only what changed. */
private[ui] final class StatusLine(
  screen: Screen,
  busy: () => Boolean,
  popupOpen: () => Boolean,
  queuedInputs: () => Int,
):
  import screen.{g, plain, terminal, width}

  private val footer = if plain then None else Option(Status.getStatus(terminal))
  private var footerSize = (0, 0)
  private var lastFooter = ""
  private var context = ""
  private var titleBase = "atc"
  private var lastTitle = ""
  /** The program status between turns: `idle` until a turn ends. */
  private var resting: StatusLine.Program = ("idle", "", "")
  private var lastStatus: StatusLine.Program = ("", "", "")
  @volatile private var closed = false
  @volatile private var turnStarted = 0L
  /** What the turn is doing ("reasoning", "running Scala"). */
  @volatile var operation = "ready"
  /** A correction typed during the turn and not sent yet. */
  @volatile var draft = ""
  /** The keys the open menu or answer field takes; shown instead of everything else. */
  @volatile private var hint = ""
  /** What the open pop-up waits for, as the program status `kind` and message. */
  @volatile var waitingFor: (kind: String, message: String) = ("question", "")

  footer.foreach: status =>
    status.setBorder(false)
    // Reserve the footer before writing content; growing it later scrolls the first lines away.
    StatusLine.draw(terminal, status, "")
  if !plain then screen.frame(screen.writeStyle(s"${Ansi.Esc}[22;0t"))

  /** Whether there is a footer to show progress in (otherwise a spinner line shows it). */
  def shown: Boolean = footer.isDefined

  def setContext(label: String, title: String): Unit =
    context = label
    titleBase = title
    refresh()

  /** A turn starts: the elapsed time counts from now. */
  def beginTurn(): Unit =
    turnStarted = System.nanoTime()
    operation = "starting turn"
    refresh()

  def setOperation(label: String): Unit =
    if operation != label then
      operation = label
      refresh()

  def withOperation[A](label: String)(body: => A): A = showing(operation, operation = _, label)(body)

  def withHint[A](keys: String)(body: => A): A = showing(hint, hint = _, keys)(body)

  /** Run `body` with the footer field that `set` changes showing `value`, then what it held before. */
  private def showing[A](current: String, set: String => Unit, value: String)(body: => A): A =
    def show(text: String): Unit = screen.synchronized:
      set(text)
      refresh()
    show(value)
    try body
    finally show(current)

  /** Redraw the window title and send the program status where they changed. */
  def refreshTitle(): Unit = if !plain && !closed then
    val waiting = busy() && popupOpen()
    val marker = if waiting then "? " else if busy() then s"${g.bullet} " else ""
    val title = Ansi.sanitize(marker + titleBase).replace('\n', ' ').replace('\t', ' ')
    val status: StatusLine.Program =
      if waiting then ("blocked", waitingFor.kind, waitingFor.message)
      else if busy() then ("working", "", operation)
      else resting
    if title != lastTitle || status != lastStatus then
      if title != lastTitle then screen.writeStyle(s"${Ansi.Esc}]0;$title${Ansi.Bel}")
      if status != lastStatus then
        screen.writeStyle(StatusLine.programStatus(status.state, status.kind, status.message))
      lastTitle = title
      lastStatus = status
      screen.flush()

  /** Report how a turn ended until the next one starts: `done` with `summary` when it
    * finished, `error` when it stopped otherwise, `idle` after an interruption or for
    * work that is not a turn (`None`). */
  def turnEnded(result: Option[(TurnOutcome, String)]): Unit =
    resting = result match
      case Some((TurnOutcome.Finished, summary)) => ("done", "", summary)
      case Some((TurnOutcome.Interrupted, _)) | None => ("idle", "", "")
      case Some((_, summary)) => ("error", "", summary)

  /** Redraw the title and the footer where they changed; the footer also after a resize,
    * at the size the screen measured when the terminal said it changed. */
  def refresh(): Unit =
    refreshTitle()
    if !closed then
      footer.foreach: status =>
        val dimensions = (width, screen.height)
        val resized = dimensions != footerSize
        if resized then
          status.resize(Size.of(width, screen.height))
          footerSize = dimensions
        val text = screen.fit(Ansi.sanitize(label).replace('\t', ' '), 0)
        if resized || text != lastFooter then
          screen.flush()
          StatusLine.draw(terminal, status, text)
          lastFooter = text

  /** Draw the footer again from nothing, after the screen was cleared under it. */
  def redraw(): Unit =
    footer.foreach(_.reset())
    footerSize = (0, 0)
    lastFooter = ""
    refresh()

  private def label: String =
    val elapsed = if busy() then s" ${Format.duration((System.nanoTime() - turnStarted) / 1e9)}" else ""
    val queued = queuedInputs()
    val waiting = if queued > 0 then s" ${g.dot} ${Format.plural(queued, "message")} queued" else ""
    if hint.nonEmpty then hint
    else if draft.nonEmpty then s"Update: ${draft.takeRight((width - 30).max(10))} ${g.dot} Enter to send$waiting"
    else if busy() then
      val frame = g.spinner(((System.nanoTime() - turnStarted) / 100_000_000L % g.spinner.length).toInt)
      s"$frame ${operation.take((width / 2).max(20))}$elapsed$waiting ${g.dot} $context"
    else context

  /** Restore the terminal's title, clear the program status and take the footer down;
    * later updates draw nothing. */
  def close(): Unit =
    closed = true
    if !plain then screen.synchronized(screen.writeStyle(s"${Ansi.Esc}[23;0t${Ansi.Esc}]7501;state=clear${Ansi.Bel}"))
    screen.synchronized(screen.flush())
    footer.foreach(_.close())

private[atc] object StatusLine:
  /** A Program Status Protocol report's fields. */
  private type Program = (state: String, kind: String, message: String)

  /** A Program Status Protocol report (OSC 7501): `key=value` pairs, the message on one
    * line and Base64-encoded. Terminals without the protocol ignore it. */
  private[atc] def programStatus(state: String, kind: String, message: String): String =
    val text = Base64.getEncoder.nn.encodeToString(Notifier.clean(message).getBytes(UTF_8)).nn
    List("state" -> state, "kind" -> kind, "app" -> "atc", "msg" -> text)
      .collect { case (key, value) if value.nonEmpty => s"$key=$value" }
      .mkString(s"${Ansi.Esc}]7501;", ":", Ansi.Bel)

  /** Draw the one-line footer and flush it. JLine's `Status.update` flushes the text itself
    * but leaves the closing synchronized-update sequence (`ESC[?2026l`) in the buffered
    * writer, and a terminal honouring mode 2026 (xterm.js/VS Code, iTerm2, kitty, Ghostty,
    * WezTerm) keeps rendering frozen until something else flushes: each footer repainted
    * from the input-poll clock would stall the whole window for up to a poll interval. */
  private[atc] def draw(terminal: Terminal, status: Status, text: String): Unit =
    status.update(List(AttributedString(text)).asJava)
    terminal.writer().flush()
