package atc.ui

import java.util.regex.{Matcher, Pattern}
import scala.collection.mutable

/** The long pastes of one read. A paste of many lines or characters would fill the
  * prompt and scroll the conversation away, so the prompt shows a placeholder for it
  * (`[Pasted text #1: 120 lines]`) and the read returns the text itself in its place.
  * A placeholder is ordinary buffer text: it can be moved, killed and yanked whole, and
  * one that was edited no longer stands for anything and is sent as it reads. */
private[ui] final class PastedText:
  /** Placeholder to pasted text, in the order pasted. */
  private val held = mutable.LinkedHashMap[String, String]()

  /** Forget the pastes: a new read begins. */
  def clear(): Unit = held.clear()

  /** The placeholder to show instead of the pasted `text`, when it is long
    * ([[PastedText.isLong]]); a short paste is shown as it is. */
  def collapse(text: String): Option[String] = Option.when(PastedText.isLong(text)):
    val placeholder = PastedText.placeholder(held.size + 1, text)
    held(placeholder) = text
    placeholder

  /** `line` with every placeholder replaced by its text. One pass, so pasted text that
    * holds what reads as a placeholder stays as it was pasted. */
  def expand(line: String): String =
    pattern.fold(line)(_.matcher(line).nn.replaceAll(found => Matcher.quoteReplacement(held(found.group().nn))).nn)

  /** The placeholder that ends at `cursor` in `line`, which Backspace removes whole. */
  def endingAt(line: String, cursor: Int): Option[String] =
    held.keysIterator.find(placeholder => line.startsWith(placeholder, cursor - placeholder.length))

  /** What matches the placeholders, to expand and to highlight them; `None` without any. */
  def pattern: Option[Pattern] =
    Option.when(held.nonEmpty)(Pattern.compile(held.keysIterator.map(Pattern.quote).mkString("|")).nn)

private[ui] object PastedText:
  /** A paste with more characters or more lines than these gets a placeholder. */
  val MaxChars = 800
  val MaxLines = 3

  private def lines(text: String): Int = text.linesIterator.size

  def isLong(text: String): Boolean = text.length > MaxChars || lines(text) > MaxLines

  /** The placeholder of the `number`th paste of a read: its lines, or its characters when it is one line. */
  def placeholder(number: Int, text: String): String =
    val size =
      if lines(text) > 1 then Format.plural(lines(text), "line")
      else Format.plural(text.codePointCount(0, text.length), "char")
    s"[Pasted text #$number: $size]"
