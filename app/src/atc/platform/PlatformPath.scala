package atc.platform

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileSystemException, Files, LinkOption, Path, Paths}
import java.util.Locale

/** Host-path parsing, display and validation. Agent-facing paths always use
  * forward slashes; input is converted to native separators only where the
  * current filesystem requires it. */
private[atc] object PlatformPath:
  val userHome: Path = Paths.get(scala.util.Properties.userHome).nn

  /** Stable path text exposed to agent code and shown in prompts. */
  def portable(path: Path): String =
    val native = path.toString
    if Platform.isWindows then native.replace('\\', '/') else native

  /** [[portable]] text for messages to the user: under `~` when inside the home directory. */
  def display(path: Path): String =
    if path == userHome then "~"
    else if path.startsWith(userHome) then "~/" + portable(userHome.relativize(path).nn)
    else portable(path)

  /** Convert slash-based path text to the current filesystem's spelling. */
  def native(value: String): String =
    if Platform.isWindows then value.replace('/', Platform.fileSeparator) else value

  /** Normalize path text for slash-based matching without changing literal
    * backslashes on filesystems where they are ordinary filename characters. */
  def slashSeparated(value: String): String =
    if Platform.isWindows then value.replace('\\', '/') else value

  def hasSeparator(value: String): Boolean = value.exists(isSeparator)

  def isRoot(value: String): Boolean =
    value == "/" ||
      (Platform.isWindows && (value == "\\" || value.matches("(?i)^[a-z]:[\\\\/]$")))

  /** Remove path separators from the end. A root loses its separator too, so call
    * [[isRoot]] first when roots have special meaning. */
  def stripTrailingSeparators(value: String): String =
    value.take(value.lastIndexWhere(!isSeparator(_)) + 1)

  private inline def isSeparator(char: Char): Boolean =
    char == '/' || (Platform.isWindows && char == '\\')

  /** Resolve `~` and `~/...` using the process home directory. Both slash
    * spellings are accepted after `~`, on every platform. */
  def expandHome(value: String): String =
    if value == "~" then userHome.toString
    else if value.startsWith("~/") || value.startsWith("~\\") then
      val relative = value.drop(2).map(c => if c == '/' || c == '\\' then Platform.fileSeparator else c)
      userHome.resolve(relative).toString
    else value

  /** Parse path text from the user or the agent: expand `~`, refuse what [[validationError]]
    * refuses before `java.nio` sees the text, and convert it to a native `Path`, which may
    * be relative. Throws `IllegalArgumentException`. */
  def parse(value: String): Path =
    val expanded = expandHome(value)
    validationError(expanded).foreach: reason =>
      throw IllegalArgumentException(s"Invalid Windows path '$value': $reason")
    Paths.get(native(expanded)).nn

  /** Why a path is unsafe on the current platform, if it is unsafe. */
  def validationError(value: String): Option[String] =
    if Platform.isWindows then windowsValidationError(value) else None

  /** Drive-relative paths must be rejected even when their suffix contains a
    * glob and therefore cannot undergo full literal-path validation. */
  def driveRelativeValidationError(value: String): Option[String] =
    Option.when(Platform.isWindows && isDriveRelative(value))(DriveRelativeError)

  /** Why a path is unsafe under Win32 name resolution. Kept string-based so
    * callers can reject device namespaces before java.nio touches them. */
  def windowsValidationError(value: String): Option[String] =
    val normalized = value.replace('/', '\\')
    val lower = normalized.toLowerCase(Locale.ROOT)
    if lower.startsWith("\\\\.\\") || lower.startsWith("\\\\?\\") || lower.startsWith("\\??\\") then
      Some("Win32 device namespaces are not allowed")
    else if isDriveRelative(normalized) then Some(DriveRelativeError)
    else
      normalized.split("\\\\+", -1).iterator
        .filterNot(c => c.isEmpty || c == "." || c == ".." || c.matches("(?i)[a-z]:")) // not file names
        .flatMap: component =>
          if component.contains(':') then Some("alternate data streams are not allowed")
          else if component.endsWith(".") || component.endsWith(" ") then
            Some("path components ending in a dot or space are not allowed")
          else
            val stem = component.takeWhile(_ != '.').reverse.dropWhile(c => c == '.' || c == ' ').reverse
              .toUpperCase(Locale.ROOT)
            val reserved =
              Set("CON", "PRN", "AUX", "NUL", "CONIN$", "CONOUT$", "CLOCK$").contains(stem) ||
                stem.matches("COM[1-9¹²³]") || stem.matches("LPT[1-9¹²³]")
            Option.when(reserved)(s"'$component' is a reserved Windows device name")
        .nextOption()

  /** A Windows path on a network share (`\\server\share\...`). */
  def isUnc(path: Path): Boolean =
    Platform.isWindows && Option(path.getRoot).exists(_.toString.startsWith("\\\\"))

  private val DriveRelativeError = "drive-relative paths such as 'C:work' are ambiguous; use 'C:/work'"

  private def isDriveRelative(value: String): Boolean =
    value.replace('/', '\\').matches("(?i)^[a-z]:(?:$|[^\\\\].*)")

  /** Whether `path` itself is a symbolic link or another reparse point (a Windows junction
    * is not reported as a link, but its no-follow attributes are `isOther`). */
  def isLink(path: Path): Boolean =
    try
      val attributes = Files.readAttributes(path, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS).nn
      attributes.isSymbolicLink || attributes.isOther
    catch case _: IOException => false

  /** Convert a path to absolute normalized form with every symbolic link resolved: the path
    * the policy evaluates and the operation then opens. Dangling links are resolved too,
    * because writing through one creates its target. No name of the result is a link the OS
    * would follow, so what is opened is what was judged; a path that cannot be resolved that
    * far, a link loop or a chain past [[MaxLinkDepth]], is refused with a `FileSystemException`.
    *
    * The OS resolves a path that exists. Otherwise a name is missing, a link dangles, or the
    * kernel gave up on the links (it follows 32 to 40 in one lookup): then the nearest
    * ancestor it does resolve is real, and each name below is examined under a real parent,
    * where a lookup has no link left to follow and so cannot fail for their number. */
  def canonical(path: Path): Path =
    // The names below `current` still to be resolved, the next one first.
    val pending = java.util.ArrayDeque[Path]()
    var current = path.toAbsolutePath.normalize
    var real = false
    while !real do
      try
        current = current.toRealPath()
        real = true
      catch
        case _: IOException =>
          val parent = current.getParent
          val name = current.getFileName
          if parent == null || name == null then real = true // a root that does not resolve
          else
            pending.push(name)
            current = parent
    var followed = 0
    while !pending.isEmpty do
      val name = pending.pop().nn
      name.toString match
        case "." => ()
        case ".." => current = Option(current.getParent).getOrElse(current)
        case _ =>
          val candidate = current.resolve(name).nn
          try current = candidate.toRealPath()
          catch
            case _: IOException if Files.isSymbolicLink(candidate) =>
              followed += 1
              if followed > MaxLinkDepth then
                throw FileSystemException(path.toString, null, "Too many levels of symbolic links")
              val target = Files.readSymbolicLink(candidate).nn
              // A target with a root starts over from that root; a relative one continues here.
              // Windows takes `..` in a relative target as removing the name before it, not as
              // the parent of where that name leads, so there it starts over from the root too.
              val names =
                if target.getRoot != null then candidate.resolveSibling(target).nn
                else if Platform.isWindows then candidate.resolveSibling(target).nn.normalize.nn
                else target
              if names.getRoot != null then current = names.getRoot
              for i <- names.getNameCount - 1 to 0 by -1 do pending.push(names.getName(i))
            case _: IOException => current = candidate // a missing name
    current

  /** Links [[canonical]] follows itself, besides those the OS resolves. */
  private val MaxLinkDepth = 40
