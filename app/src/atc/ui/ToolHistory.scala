package atc.ui

import atc.host.FileChange
import atc.sandbox.ExecutionResult

/** Recent model-visible output and file previews. Classified terminal text is never stored.
  * The count matches the default `maxToolCalls`, so a whole turn's calls stay reachable;
  * memory is bounded by `maxChars`. */
private[ui] final class ToolHistory(maxChars: Int = 8 * 1024 * 1024, maxEntries: Int = 200):
  import ToolHistory.Entry
  require(maxChars > 0 && maxEntries > 0, "Tool history limits must be positive")

  private var retained = Vector.empty[Entry]
  private var nextId = 0
  private def cap(text: String, limit: Int): String =
    if text.length <= limit then text else text.take(limit) + "\n[retained output limit reached]"

  /** Keep a finished call. The result and the command output share one output limit. */
  def add(
    title: String,
    code: String,
    result: ExecutionResult,
    millis: Long,
    live: String,
    changes: List[FileChange]
  ): Entry =
    nextId += 1
    val limit = math.min(maxChars / 2, 2 * 1024 * 1024)
    val output = cap(result.render, limit)
    val entry = Entry(
      nextId,
      title,
      cap(code, 32000),
      output,
      cap(live, (limit - output.length).max(0)),
      result.success,
      millis,
      changes.take(50).map(c => c.copy(preview = cap(c.preview, 4000)))
    )
    retained :+= entry
    while retained.size > maxEntries || retained.size > 1 && retained.map(_.size).sum > maxChars do
      retained = retained.tail
    entry

  /** The retained calls, oldest first. */
  def entries: Vector[Entry] = retained
  def get(id: Int): Option[Entry] = retained.find(_.id == id)
  def latest: Option[Entry] = retained.lastOption
  def clear(): Unit = retained = Vector.empty

private[ui] object ToolHistory:
  /** One finished call: `output` is what the REPL returned, without the hints and the cut
    * of the model's copy, and `live` is what the commands it ran wrote. */
  final case class Entry(
    id: Int,
    title: String,
    code: String,
    output: String,
    live: String,
    success: Boolean,
    millis: Long,
    changes: List[FileChange]
  ):
    def size: Int = code.length + output.length + live.length +
      changes.map(c => c.path.length + c.summary.length + c.preview.length).sum
