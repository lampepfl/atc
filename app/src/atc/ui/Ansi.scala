package atc.ui

import scala.util.matching.Regex

/** The terminal escape sequences the UI writes. They are written by hand: JLine's
  * `toAnsi(terminal)` would rewrite box-drawing glyphs into the DEC charset or plain ASCII.
  * The named codes are the SGR parameters the front-end uses, by role. */
object Ansi:
  val Esc = "\u001b"
  /** Rings the bell; also ends an OSC sequence. */
  val Bel = "\u0007"
  val Reset: String = Esc + "[0m"
  /** Back to the start of the current line and clear it. */
  val ClearLine: String = "\r" + Esc + "[2K"
  /** Bracketed paste on and off: the terminal marks a paste with `ESC[200~` and `ESC[201~`. */
  val PasteOn: String = Esc + "[?2004h"
  val PasteOff: String = Esc + "[?2004l"
  val HideCursor: String = Esc + "[?25l"
  val ShowCursor: String = Esc + "[?25h"

  val Bold = 1
  val Dim = 2
  val Red = 31
  val Green = 32
  val Yellow = 33 // warnings, permissions, classified
  val Blue = 34 // TODO panel, fenced code in prose
  val Magenta = 35 // tool calls
  val Cyan = 36 // the user, questions, inline code in prose

  /** The SGR sequence selecting `codes`. */
  def sgr(codes: Int*): String = s"$Esc[${codes.mkString(";")}m"

  /** `s` wrapped in `codes` and a reset; the empty string stays empty. */
  def styled(s: String, codes: Int*): String = if s.isEmpty then s else sgr(codes*) + s + Reset

  /** Matches one SGR sequence. */
  val Sgr: Regex = """\u001b\[[0-9;]*m""".r

  /** Strip terminal control from untrusted text (model prose, program output,
    * file content, paths): C0 controls except `\n` and `\t`, DEL, C1, and the
    * Unicode bidirectional-formatting controls used for visual spoofing. ESC is
    * dropped outright, so an escape sequence straddling two streamed chunks is
    * broken as well. Applied where such text enters the TUI, never to the TUI's
    * own styled output. */
  def sanitize(s: String): String =
    if !s.exists(isControl) then s
    else s.filterNot(isControl)

  private inline def isControl(c: Char): Boolean =
    (c < ' ' && c != '\n' && c != '\t') ||
      c == '\u007f' ||
      (c >= '\u0080' && c <= '\u009f') ||
      c == '\u061c' || c == '\u200e' || c == '\u200f' ||
      (c >= '\u202a' && c <= '\u202e') ||
      (c >= '\u2066' && c <= '\u2069')
