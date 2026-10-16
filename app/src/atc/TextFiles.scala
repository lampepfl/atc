package atc

import scala.collection.mutable

/** Small, platform-independent text conventions. Line splitting accepts LF, CRLF,
  * and bare CR; rewriting uses the first ending found so mixed files have a stable
  * convention. Character cuts keep surrogate pairs whole. Public because the REPL
  * rendering in `dotty.tools.repl` uses it. */
object TextFiles:
  val DefaultLineEnding = "\n"

  /** The first `n` characters of `text`, one fewer where the cut would split a surrogate pair:
    * a lone surrogate kept in history can make a provider reject every later request. */
  def takeChars(text: String, n: Int): String =
    if n > 0 && n < text.length && Character.isHighSurrogate(text.charAt(n - 1)) then text.take(n - 1)
    else text.take(n)

  /** The last `n` characters of `text`, likewise without half a surrogate pair. */
  def takeRightChars(text: String, n: Int): String =
    val tail = text.takeRight(n)
    if tail.length < text.length && tail.nonEmpty && Character.isLowSurrogate(tail.charAt(0)) then tail.drop(1)
    else tail

  /** Number of characters occupied by a leading Unicode byte-order mark. */
  def bomLength(text: String): Int =
    if text.nonEmpty && text.charAt(0) == '\uFEFF' then 1 else 0

  /** Drop one leading Unicode byte-order mark, leaving embedded marks alone. */
  def stripBom(text: String): String = text.substring(bomLength(text))

  /** One line ending and its position in the containing string. */
  final case class LineEnding(index: Int, text: String)

  def firstLineEnding(text: String): Option[LineEnding] =
    text.indices.iterator.flatMap(lineEndingAt(text, _)).nextOption()

  /** The text of the first line ending, or [[DefaultLineEnding]] when there is none. */
  def lineEnding(text: String): String = firstLineEnding(text).fold(DefaultLineEnding)(_.text)

  def lastLineEnding(text: String): Option[LineEnding] =
    text.indices.reverseIterator.flatMap { index =>
      // The `\n` of a `\r\n` belongs to the ending that starts one character earlier.
      lineEndingAt(text, index - 1).filter(_.text == "\r\n").orElse(lineEndingAt(text, index))
    }.nextOption()

  /** Lines plus enough source formatting to join them again. A mixed-ending
    * input is normalized to its first ending when joined. */
  final case class LineSplit(lines: List[String], lineEnding: String, trailingLineEnding: Boolean):
    def join: String =
      if lines.isEmpty then "" else lines.mkString(lineEnding) + (if trailingLineEnding then lineEnding else "")

  def splitLines(text: String): LineSplit =
    if text.isEmpty then LineSplit(Nil, DefaultLineEnding, trailingLineEnding = true)
    else
      val lines = mutable.ListBuffer[String]()
      var firstEnding: Option[String] = None
      var start = 0
      var index = 0
      while index < text.length do
        lineEndingAt(text, index) match
          case None => index += 1
          case Some(ending) =>
            if firstEnding.isEmpty then firstEnding = Some(ending.text)
            lines += text.substring(start, index)
            index += ending.text.length
            start = index
      val trailing = start == text.length
      if !trailing then lines += text.substring(start)
      LineSplit(lines.toList, firstEnding.getOrElse(DefaultLineEnding), trailing)

  /** Append one logical line, preserving the existing text and its first line
    * ending. A missing final ending is supplied; an existing one is replaced
    * only when a mixed-ending file ends differently from its first ending. */
  def appendLine(text: String, line: String): String =
    val ending = lineEnding(text)
    val prefix =
      if text.isEmpty then ""
      else
        lastLineEnding(text).filter(e => e.index + e.text.length == text.length) match
          case Some(last) => text.substring(0, last.index) + ending
          case None => text + ending
    prefix + line + ending

  private def lineEndingAt(text: String, index: Int): Option[LineEnding] =
    if index < 0 then None
    else
      text.charAt(index) match
        case '\r' if index + 1 < text.length && text.charAt(index + 1) == '\n' =>
          Some(LineEnding(index, "\r\n"))
        case '\r' | '\n' => Some(LineEnding(index, text.charAt(index).toString))
        case _ => None
