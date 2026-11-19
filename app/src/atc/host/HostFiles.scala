package atc.host

import atc.{ScalaSource, TextFiles}
import atc.lib.*
import atc.platform.{PathGlob, Platform, PlatformPath}

import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}
import java.nio.file.Paths
import java.util.regex.{Matcher, Pattern}
import scala.collection.mutable
import scala.util.Using
import scala.util.matching.Regex

/** The agent-facing file operations supplied by [[Host]]. They work through
  * [[FileEntryImpl]] handles, so every path passes the checks in [[HostPaths]]. */
private[host] trait HostFiles:
  self: Host =>

  def requestFiles[T, C <: caps.CapSet](path: String)(using UserIO, FileSystem)(op: FileSystem ?=> T): T =
    requestFiles(path, Access.Read, "")(op)

  def requestFiles[T, C <: caps.CapSet](path: String, access: Access)(using
    UserIO,
    FileSystem
  )(
    op: FileSystem ?=> T
  ): T =
    requestFiles(path, access, "")(op)

  def requestFiles[T, C <: caps.CapSet](path: String, access: Access, reason: String)(using
    user: UserIO,
    parent: FileSystem
  )(op: FileSystem ?=> T): T =
    val requestedAccess = access match
      case Access.Read => atc.perms.Access.Read
      case Access.Write => atc.perms.Access.Write
    inScope(policy.requestFile(scopeOf(parent), canonical(path), requestedAccess, reason)): id =>
      op(using FileSystemImpl(id, this))

  def access(path: String)(using fs: FileSystem): FileEntry = fs.access(path)

  def read(path: String)(using fs: FileSystem): String = fs.access(path).read()

  def readLines(path: String)(using fs: FileSystem): List[String] = fs.access(path).readLines()

  def readRange(path: String, from: Int, to: Int)(using fs: FileSystem): String =
    if from < 1 || to < from || to.toLong - from >= 1000 then
      throw IllegalArgumentException(
        "readRange: use an inclusive range of 1 to 1000 lines, starting at line 1 or later"
      )
    val (kept, _, limited) = window(impl(fs.access(path)), "readRange", from, to, to)
    (kept.map(_.shown) ++ Option.when(limited)(ReadLimitNote)).mkString("\n")

  /** Print a numbered view capped at [[Host.CatMaxLines]]. Lines are streamed,
    * so a large file or line is never loaded whole: only the shown prefixes
    * are kept. */
  def cat(path: String)(using fs: FileSystem, user: UserIO): Unit =
    val entry = impl(fs.access(path))
    val kept = mutable.ListBuffer[CappedLine]()
    var lineCount = 0
    val cut = entry.scanLines("cat", Host.CatMaxLineChars, Host.CatMaxReadChars): (prefix, chars, number) =>
      lineCount = number
      if lineCount <= Host.CatMaxLines then kept += CappedLine(prefix, chars)
      true
    val text =
      if lineCount == 0 then "[empty file]\n"
      else
        val body = numbered(kept.toList, 1)
        val more = if cut then "+" else ""
        if lineCount <= Host.CatMaxLines && !cut then body
        else
          val next = math.min(math.max(lineCount, Host.CatMaxLines + 1), 2 * Host.CatMaxLines)
          val continuation = s"cat(${ScalaSource.stringLiteral(path)}, ${Host.CatMaxLines + 1}, $next)"
          val remaining = (lineCount - Host.CatMaxLines).max(0)
          body + s"... [$remaining$more more lines ($lineCount$more in all): $continuation shows the next]\n"
    output.print(text, text)

  /** Print an inclusive, one-based range, bounded like the default file preview. */
  def cat(path: String, from: Int, to: Int)(using fs: FileSystem, user: UserIO): Unit =
    if from < 1 || to < from then
      throw IllegalArgumentException(s"cat: the range must satisfy 1 <= from <= to (got $from, $to)")
    val last = to.toLong.min(from.toLong + Host.CatMaxLines - 1).toInt
    val (kept, lineCount, limited) = window(impl(fs.access(path)), "cat", from, last, to)
    val text =
      if limited then numbered(kept, from) + ReadLimitNote + "\n"
      else if from > lineCount then s"[nothing to show: $path has $lineCount lines]\n"
      else
        val body = numbered(kept, from)
        if lineCount > last then
          body + s"... [more lines: cat(${ScalaSource.stringLiteral(path)}, ${last + 1}, $to) continues]\n"
        else if to > lineCount then body + s"[end of file: $lineCount lines]\n"
        else body
    output.print(text, text)

  /** The retained prefix of a line and the line's full length. */
  private final case class CappedLine(prefix: String, chars: Long):
    /** The prefix, marked with the number of characters cut from it. */
    def shown: String = if chars == prefix.length then prefix else s"$prefix ... [+${chars - prefix.length} chars]"

  private val ReadLimitNote = "[read limit reached before completing the requested range]"

  /** Lines `from` to `last` of `entry`, the number of lines scanned, and whether the read limit cut
    * the scan short. When `last < to`, one more line is scanned to tell whether the range goes on. */
  private def window(
    entry: FileEntryImpl,
    operation: String,
    from: Int,
    last: Int,
    to: Int
  ): (List[CappedLine], Int, Boolean) =
    val kept = mutable.ListBuffer[CappedLine]()
    var lineCount = 0
    val limited = entry.scanLines(operation, Host.CatMaxLineChars, Host.ReadRangeMaxChars): (prefix, chars, number) =>
      lineCount = number
      if number >= from && number <= last then kept += CappedLine(prefix, chars)
      number < to && number <= last
    (kept.toList, lineCount, limited)

  private def numbered(lines: List[CappedLine], first: Int): String =
    val result = StringBuilder()
    for (line, index) <- lines.zipWithIndex do result.append(f"${first + index}%6d\t").append(line.shown).append('\n')
    result.toString

  def readBytes(path: String)(using fs: FileSystem): Array[Byte] = fs.access(path).readBytes()

  def write(path: String, content: String)(using fs: FileSystem): Unit = fs.access(path).write(content)

  def writeBytes(path: String, content: Array[Byte])(using fs: FileSystem): Unit =
    fs.access(path).writeBytes(content)

  /** Move through checked primitives so the operation grants no extra access.
    * Streamed: a large file is copied in chunks rather than read whole. */
  def move(from: String, to: String)(using fs: FileSystem): Unit =
    val source = impl(fs.access(from))
    if source.isDirectory then
      throw IllegalArgumentException(s"move: '$from' is a directory; move its files and mkdir/delete the directories")
    val target = impl(fs.access(to))
    // Compared as resolved: a target that is a link to the source names the same file, which
    // deleting the source afterwards would lose.
    source.checkedRemovable("move")
    if source.canonicalPath != target.canonicalPath then
      Using.resource(source.openRead())(in => target.writeFrom(source, in))
      source.delete()
    else
      // Where names ignore case, a new spelling of the same name is the same file, and the
      // move would do nothing: say so rather than report a rename that did not happen.
      val wanted = Option(PlatformPath.parse(to).getFileName).map(_.toString)
      val actual = Option(target.canonicalPath.getFileName).map(_.toString)
      val caseOnly = wanted.exists(w => actual.exists(a => a != w && a.equalsIgnoreCase(w)))
      // A target that is a link to the source is another name, not another spelling.
      if caseOnly && Platform.caseInsensitivePaths && !PlatformPath.isLink(target.removable) then
        throw IllegalArgumentException(
          s"move: '$from' and '$to' name the same file on this file system; to change only the case of its name, move it to a temporary name first"
        )

  def copy(from: String, to: String)(using fs: FileSystem): Unit =
    val source = impl(fs.access(from))
    if source.isDirectory then
      throw IllegalArgumentException(s"copy: '$from' is a directory; copy its files one by one")
    Using.resource(source.openRead())(in => impl(fs.access(to)).writeFrom(source, in))

  /** The host's own [[FileEntry]] implementation; agent code cannot supply
    * another, so the cast is safe and keeps streaming helpers off the public API. */
  private def impl(entry: FileEntry): FileEntryImpl = entry match
    case e: FileEntryImpl => e
    case other => throw SecurityException(s"Unknown FileEntry implementation: ${other.getClass.getName}")

  /** The text of a file that an edit rewrites as a whole, with its byte-order mark apart, so
    * that the mark stays in front and is not part of line 1. A file that is not valid UTF-8
    * is refused: `read` shows its other bytes as U+FFFD, and writing that text back would
    * replace them on lines the edit never touched. */
  private def editable(entry: FileEntry, operation: String): (String, String) =
    val decoder = StandardCharsets.UTF_8.newDecoder().nn
      .onMalformedInput(CodingErrorAction.REPORT).nn.onUnmappableCharacter(CodingErrorAction.REPORT).nn
    val text =
      try decoder.decode(ByteBuffer.wrap(entry.readBytes())).toString
      catch
        case _: CharacterCodingException =>
          throw IllegalArgumentException(
            s"$operation: '${entry.path}' is not valid UTF-8, and rewriting it as text would change its other bytes; use readBytes and writeBytes"
          )
    text.splitAt(TextFiles.bomLength(text))

  /** Rewrite every regex match in place and reject an accidental no-op. One
    * pass both counts and rewrites, so a large file is scanned once. */
  def sed(path: String, pattern: String, replacement: String)(using fs: FileSystem): Int =
    if pattern.isEmpty then throw IllegalArgumentException("sed: the pattern must not be empty")
    val regex = Pattern.compile(pattern, Pattern.MULTILINE)
    val entry = fs.access(path)
    val (bom, before) = editable(entry, "sed")
    val javaReplacement = sedReplacement(replacement)
    val rewritten = java.lang.StringBuilder()
    val matcher = regex.matcher(before)
    var count = 0
    while matcher.find() do
      count += 1
      matcher.appendReplacement(rewritten, javaReplacement)
    matcher.appendTail(rewritten)
    if count == 0 then
      throw IllegalArgumentException(
        s"sed: the regex '$pattern' matches nothing in '${entry.path}', so nothing was changed; check it with grep(path, pattern), and quote literal text with quote(text) (the pattern) and quoteReplacement(text) (the replacement)."
      )
    entry.write(bom + rewritten.toString)
    count

  /** Convert sed-style group and escape syntax to Java replacement syntax. */
  private def sedReplacement(replacement: String): String =
    val result = StringBuilder()
    var index = 0
    while index < replacement.length do
      val char = replacement.charAt(index)
      if char == '\\' && index + 1 < replacement.length then
        replacement.charAt(index + 1) match
          case digit if digit.isDigit => result.append('$').append(digit)
          case 'n' => result.append('\n')
          case 't' => result.append('\t')
          case other => result.append('\\').append(other)
        index += 2
      else
        if char == '\\' then result.append("\\\\") else result.append(char)
        index += 1
    result.toString

  // TODO(safe-mode): remove with the Interface declarations once safe mode admits these methods.
  def quote(text: String): String = Pattern.quote(text).nn

  def quoteReplacement(text: String): String = Matcher.quoteReplacement(text).nn

  def replaceExact(path: String, expected: String, replacement: String)(using fs: FileSystem): Unit =
    if expected.isEmpty then throw IllegalArgumentException("replaceExact: expected text must not be empty")
    val entry = fs.access(path)
    val (bom, before) = editable(entry, "replaceExact")
    val index = before.indexOf(expected)
    if index < 0 then throw IllegalArgumentException("replaceExact: expected text was not found; read the file again")
    if before.lastIndexOf(expected) != index then
      throw IllegalArgumentException("replaceExact: expected text is ambiguous; include more surrounding context")
    entry.write(bom + before.substring(0, index) + replacement + before.substring(index + expected.length))

  def replaceLines(path: String, from: Int, to: Int, text: String)(using fs: FileSystem): String =
    val entry = fs.access(path)
    val (bom, before) = editable(entry, "replaceLines")
    val document = TextFiles.splitLines(before)
    val lines = document.lines
    val lineCount = lines.length
    if from < 1 || to < from || to > lineCount then
      throw IllegalArgumentException(
        s"replaceLines: the range must satisfy 1 <= from <= to <= $lineCount (the file has $lineCount lines), got $from..$to; cat the file again, line numbers shift after an edit"
      )
    val old = lines.slice(from - 1, to)
    val updated = lines.take(from - 1) ++ TextFiles.splitLines(text).lines ++ lines.drop(to)
    entry.write(bom + document.copy(lines = updated).join)
    old.mkString(document.lineEnding)

  def insertLines(path: String, before: Int, text: String)(using fs: FileSystem): Unit =
    val entry = fs.access(path)
    val (bom, existing) = editable(entry, "insertLines")
    val document = TextFiles.splitLines(existing)
    val lines = document.lines
    val lineCount = lines.length
    if before < 1 || before > lineCount + 1 then
      throw IllegalArgumentException(
        s"insertLines: `before` must be between 1 and ${lineCount + 1} (the file has $lineCount lines), got $before"
      )
    val updated = lines.take(before - 1) ++ TextFiles.splitLines(text).lines ++ lines.drop(before - 1)
    entry.write(bom + document.copy(lines = updated).join)

  def append(path: String, content: String)(using fs: FileSystem): Unit = fs.access(path).append(content)

  def exists(path: String)(using fs: FileSystem): Boolean = fs.access(path).exists

  def isDirectory(path: String)(using fs: FileSystem): Boolean = fs.access(path).isDirectory

  def mkdir(path: String)(using fs: FileSystem): Unit = fs.access(path).mkdir()

  def delete(path: String)(using fs: FileSystem): Unit = fs.access(path).delete()

  def ls(dir: String)(using fs: FileSystem): List[String] = fs.access(dir).children.map(entry => fromCwd(entry.path))

  def walk(dir: String)(using fs: FileSystem): List[String] = fs.access(dir).walk().map(entry => fromCwd(entry.path))

  private def grepEntry(entry: FileEntryImpl, operation: String, regex: Regex): List[GrepMatch] =
    val matches = mutable.ListBuffer[GrepMatch]()
    val shown = fromCwd(entry.path)
    entry.forEachLine(
      operation,
      (line, number) => if regex.findFirstIn(line).isDefined then matches += GrepMatch(shown, number, line)
    )
    matches.toList

  def grep(path: String, pattern: String)(using fs: FileSystem): List[GrepMatch] =
    grepEntry(impl(fs.access(path)), "grep", pattern.r)

  def grepRecursive(dir: String, pattern: String)(using fs: FileSystem): List[GrepMatch] =
    grepRecursive(dir, pattern, "*")

  def grepRecursive(dir: String, pattern: String, glob: String)(using fs: FileSystem): List[GrepMatch] =
    val regex = pattern.r
    filesNamed(dir, glob, classified = false).toList.flatMap(grepEntry(_, "grepRecursive", regex))

  def find(dir: String, glob: String)(using fs: FileSystem): List[String] =
    filesNamed(dir, glob, classified = true).map(entry => fromCwd(entry.path)).toList

  def search(dir: String, pattern: String, glob: String, options: SearchOptions)(using fs: FileSystem): SearchResult =
    val limits = List(
      options.maxMatches -> 10000,
      options.maxFiles -> 100000,
      options.maxLinesPerFile -> 1000000,
      options.maxLineChars -> 10000,
      options.maxCharsPerFile -> 10000000,
    )
    if limits.exists((value, max) => value < 1 || value > max) then
      throw IllegalArgumentException(
        "search: limits must be positive (maxMatches <= 10000, maxFiles <= 100000, maxLinesPerFile <= 1000000, maxLineChars <= 10000, maxCharsPerFile <= 10000000)"
      )
    val regex = pattern.r
    val entries = filesNamed(dir, glob, classified = false)
    val matches = mutable.ListBuffer[GrepMatch]()
    var scanned = 0
    // `limited` is set only where something was left out: a line cut at the
    // character cap, a line past the per-file line budget, or a match past the
    // match cap. A budget reached exactly at the end of the input cuts nothing.
    var limited = false
    while matches.size < options.maxMatches && scanned < options.maxFiles && entries.hasNext do
      val entry = entries.next()
      scanned += 1
      val readLimit =
        entry.scanLines("search", options.maxLineChars, options.maxCharsPerFile): (line, chars, number) =>
          if number > options.maxLinesPerFile then
            limited = true
            false
          else
            if chars > line.length then limited = true
            val hit = regex.findFirstIn(line).isDefined
            if hit && matches.size >= options.maxMatches then
              limited = true
              false
            else
              if hit then matches += GrepMatch(fromCwd(entry.path), number, line)
              true
      if readLimit then limited = true
    SearchResult(matches.toList, scanned, limited || entries.hasNext)

  /** Select the files of [[FileEntryImpl.files]] by filename or relative-path glob. */
  private def filesNamed(dir: String, glob: String, classified: Boolean)(using
    fs: FileSystem
  ): Iterator[FileEntryImpl] =
    val files = impl(fs.access(dir)).files(classified)
    val pattern = PathGlob.pattern(glob)
    if glob.contains('/') || glob.contains("**") then
      val base = canonical(dir)
      files.filter(entry => pattern.matcher(PlatformPath.portable(base.relativize(Paths.get(entry.path)).nn)).matches())
    else files.filter(entry => pattern.matcher(entry.name).matches())

  def readClassified(path: String)(using fs: FileSystem): Classified[String] = fs.access(path).readClassified()

  def writeClassified(path: String, content: Classified[String])(using fs: FileSystem): Unit =
    fs.access(path).writeClassified(content)
