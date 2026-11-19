package atc.host

import atc.lib.*
import atc.perms.{Perm, ScopeId}
import atc.platform.PlatformPath

import java.io.{InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{FileAlreadyExistsException, Files, NoSuchFileException, Path}
import java.nio.file.attribute.PosixFilePermissions
import scala.util.Using

/** Runtime capabilities carry a permission scope ID. The host checks scope,
  * configured permissions and sandbox mode on each operation. Agent-facing
  * capture types enforce the read-only/full distinction over these same objects.
  * Agent code cannot construct them: the sandbox loader shares only Scala and
  * `atc.lib` classes. */
sealed trait Scoped:
  def scope: ScopeId

final class FileSystemImpl(val scope: ScopeId, val host: Host) extends FileSystem, Scoped:
  def access(path: String): FileEntry = FileEntryImpl(this, host.entryPath(path))

/** `env` holds the variables a `withEnv` block sets for the commands run with this capability. */
final class ExecImpl(val scope: ScopeId, val env: Map[String, String] = Map.empty) extends Exec, Scoped

final class NetworkImpl(val scope: ScopeId) extends Network, Scoped

/** `at` is the path the handle names: canonical when the handle was made, or a link's own path
  * under its canonical parent. Each operation resolves it again and checks and acts on the
  * result, so a link planted there afterwards is judged by its own target, not the one checked
  * when the handle was made. Only `delete` acts on a link itself. */
final class FileEntryImpl(fs: FileSystemImpl, at: Path) extends FileEntry:
  private def host: Host = fs.host
  private def scope: ScopeId = fs.scope
  private[host] def canonicalPath: Path = PlatformPath.canonical(at)

  /** What `delete` removes: a link itself, else the current target. The parent is resolved
    * again too, so a directory swapped for a link since cannot move the link out of view. */
  private[host] def removable: Path =
    val parent = at.getParent
    val name = at.getFileName
    val here = if parent == null || name == null then at else PlatformPath.canonical(parent).resolve(name).nn
    if PlatformPath.isLink(here) then here else PlatformPath.canonical(here)

  /** [[removable]], checked for `operation`: write access where it is and, for a link, at what
    * it reaches too. A rule names what a link reaches (patterns are resolved when loaded), so
    * replacing the link would otherwise get past a read-only rule on its target. */
  private[host] def checkedRemovable(operation: String): (Path, Perm) =
    val p = removable
    val permission = host.requireWrite(scope, p, operation)
    if PlatformPath.isLink(p) then host.requireWrite(scope, PlatformPath.canonical(p), operation)
    (p, permission)

  /** The current target, checked to be readable and not classified. */
  private def readable(operation: String, alternative: String): Path =
    val p = canonicalPath
    host.requireReadable(scope, p, operation, alternative)
    p

  /** Run `op` (a read of classified content) as a `Classified` result: the
    * permission check and any failure, fatal errors included, stay inside the
    * classified value. */
  private def asClassified[T](operation: String)(op: Path => T): Classified[T] =
    val result = ClassifiedImpl.attempt:
      val p = canonicalPath
      host.requireRead(scope, p, operation)
      op(p)
    ClassifiedImpl(result)

  def path: String = PlatformPath.portable(at)

  def name: String = Option(at.getFileName).map(_.toString).getOrElse(at.toString)

  def exists: Boolean =
    val p = canonicalPath
    host.requireRead(scope, p, "exists")
    Files.exists(p)

  def isDirectory: Boolean =
    val p = canonicalPath
    host.requireRead(scope, p, "isDirectory")
    Files.isDirectory(p)

  def isClassified: Boolean = host.requireRead(scope, canonicalPath, "isClassified").classified

  def size: Long = Files.size(readable("size", "readClassified()"))

  def read(): String = String(readBytes(), UTF_8)

  def readBytes(): Array[Byte] = Files.readAllBytes(readable("read", "readClassified()")).nn

  def readLines(): List[String] = read().linesIterator.toList

  /** Stream the file line by line (never loaded whole); `op` receives each
    * line with its 1-based number. Decoding is lenient like `read()`: invalid
    * UTF-8 becomes U+FFFD, so binary or Latin-1 files do not abort a search.
    * The file is closed when the iteration ends. */
  def forEachLine(op: (String, Int) => Unit): Unit = forEachLine("forEachLine", op)

  /** [[forEachLine]] naming the operation the agent called, for denials. */
  private[host] def forEachLine(operation: String, op: (String, Int) => Unit): Unit =
    scanLines(operation, Int.MaxValue) { (line, _, number) => op(line, number); true }
    ()

  /** Stream line prefixes without ever materializing a whole line. `op` gets
    * the retained prefix (at most `maxChars` characters), the line's full
    * UTF-16 character count and its one-based number, and returns whether to
    * carry on; the stream is closed as soon as it returns false. CR, LF and
    * CRLF all end a line, and a final terminator adds no phantom line.
    *
    * Reads stop after `maxReadChars` characters. The result says whether that
    * budget cut the file short, so a file ending exactly on it is not reported
    * as truncated.
    */
  private[host] def scanLines(
    operation: String,
    maxChars: Int,
    maxReadChars: Long = Long.MaxValue
  )(op: (String, Long, Int) => Boolean): Boolean =
    if maxChars < 0 then throw IllegalArgumentException(s"maxChars must be non-negative (got $maxChars)")
    val p = readable(operation, "readClassified()")
    Using.resource(InputStreamReader(Files.newInputStream(p).nn, UTF_8)): reader =>
      val input = new Array[Char](8192)
      val prefix = StringBuilder(math.min(maxChars, input.length))
      var lineChars = 0L
      var lineNumber = 0
      var afterCr = false
      var continue = true
      var consumed = 0L
      var truncated = false

      def emit(): Unit =
        lineNumber += 1
        continue = op(prefix.toString, lineChars, lineNumber)
        prefix.clear()
        lineChars = 0L

      var read = reader.read(input)
      var index = 0
      while continue && !truncated && read >= 0 do
        if index >= read then
          // Refilling past the budget is how a file ending on it is told from a cut one.
          read = reader.read(input)
          index = 0
        else if consumed >= maxReadChars then truncated = true
        else
          val char = input(index)
          if afterCr && char == '\n' then afterCr = false
          else
            afterCr = false
            if char == '\r' then
              emit()
              afterCr = true
            else if char == '\n' then emit()
            else
              if lineChars < maxChars.toLong then prefix.append(char)
              lineChars += 1
          index += 1
          consumed += 1

      if continue && lineChars > 0 then emit()
      truncated

  /** Open this file for streaming reads; checked like `read`. The caller must
    * close the returned stream. */
  private[host] def openRead(): InputStream = Files.newInputStream(readable("read", "readClassified()")).nn

  /** Stream `in` into this file; checked like `writeBytes`. When source and
    * target are two names for the same inode, all permission checks still run
    * but opening the truncating output stream is skipped. */
  private[host] def writeFrom(source: FileEntryImpl, in: InputStream): Unit =
    val p = canonicalPath
    host.requireWritable(scope, p, "writeBytes")
    val sourcePath = source.canonicalPath
    val sameFile =
      if p == sourcePath then true
      else if !Files.exists(p) then false
      else
        try Files.isSameFile(sourcePath, p)
        catch case _: NoSuchFileException => false
    if !sameFile then
      host.withFileChange(p, "copied"):
        host.ensureParent(p)
        // A new file gets the source's permissions before it gets the content, as `cp` and
        // `mv` give it: a private file stays private, and a script stays executable.
        if !Files.exists(p) then
          try Files.createFile(p, PosixFilePermissions.asFileAttribute(Files.getPosixFilePermissions(sourcePath)))
          catch case _: UnsupportedOperationException | _: FileAlreadyExistsException => ()
        Using.resource(Files.newOutputStream(p).nn)(out => in.transferTo(out))

  def write(content: String): Unit =
    host.writeFile(scope, canonicalPath, content, append = false)

  def writeBytes(content: Array[Byte]): Unit =
    host.writeFileBytes(scope, canonicalPath, content)

  def append(content: String): Unit =
    host.writeFile(scope, canonicalPath, content, append = true)

  def delete(): Unit =
    val (p, permission) = checkedRemovable("delete")
    if permission.classified then Files.delete(p)
    else host.withFileChange(p, "deleted")(Files.delete(p))

  def mkdir(): Unit =
    val p = canonicalPath
    val permission = host.requireWrite(scope, p, "mkdir")
    if permission.classified then Files.createDirectories(p)
    else host.withFileChange(p, "directory created")(Files.createDirectories(p))
    ()

  def children: List[FileEntry] =
    host.visibleChildren(scope, readable("children", "childrenClassified")).map(FileEntryImpl(fs, _))

  def walk(): List[FileEntry] =
    host.walkPaths(scope, readable("walk", "walkClassified"), intoClassified = false).map(FileEntryImpl(fs, _))

  /** The non-directory descendants `walk` lists, lazily; classified ones only when `classified`. */
  private[host] def files(classified: Boolean): Iterator[FileEntryImpl] =
    host.iteratePaths(scope, readable("walk", "walkClassified"), intoClassified = false)
      .filter(entry => !entry.isDirectory && (classified || !entry.permission.classified))
      .map(entry => FileEntryImpl(fs, entry.path))

  def readClassified(): Classified[String] =
    asClassified("readClassified")(Files.readString(_, UTF_8).nn)

  def childrenClassified: Classified[List[String]] =
    asClassified("childrenClassified")(host.visibleChildren(scope, _).map(PlatformPath.portable))

  def walkClassified(): Classified[List[String]] =
    asClassified("walkClassified")(host.walkPaths(scope, _, intoClassified = true).map(PlatformPath.portable))

  def writeClassified(content: Classified[String]): Unit =
    // Hand the raw `Try` to the host: it runs the permission and target checks before
    // it branches on success or failure, so neither the thrown exception nor the
    // target's existence can reveal a bit of the classified value.
    host.writeClassifiedFile(scope, canonicalPath, ClassifiedImpl.unwrap(content))
