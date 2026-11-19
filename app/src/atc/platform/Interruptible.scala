package atc.platform

/** Text for a regular expression to match, which gives up once its thread is interrupted. A
  * pattern from the agent, from a config or from a `.gitignore` can take longer on some text
  * than any timeout, and matching is host code, which the sandbox's stop flag does not reach.
  * The interrupt that comes with a stop does, through this: the JDK's matcher reads its input
  * one character at a time. */
private[atc] final class Interruptible(text: CharSequence) extends CharSequence:
  private var reads = 0

  def length: Int = text.length

  def charAt(index: Int): Char =
    reads += 1
    // Not on every read: the check costs more than the read.
    if (reads & 0x3ff) == 0 && Thread.currentThread.nn.isInterrupted then
      throw InterruptedException("interrupted while matching a pattern")
    text.charAt(index)

  def subSequence(start: Int, end: Int): CharSequence = Interruptible(text.subSequence(start, end).nn)

  override def toString: String = text.toString
