package atc.host

import atc.{Debug, ScalaSource}
import atc.perms.{Perm, ScopeId}
import atc.platform.PlatformPath

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, LinkOption, Path, Paths, StandardOpenOption}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}
import scala.util.control.NonFatal

/** Path resolution, permission checks and the checked primitives behind [[FileEntryImpl]] and
  * command redirection. Paths are canonicalized by [[canonical]] before any policy check. */
private[host] trait HostPaths:
  self: Host =>

  /** Resolve a path against the host working directory and canonicalize it for
    * policy evaluation, including symlinks and dangling write targets. */
  private[atc] def canonical(path: String): Path = PlatformPath.canonical(absolute(path))

  /** The path a `FileEntry` names: canonical, except that a final link stays itself under its
    * canonical parent, so that `delete` removes the link and not its target. */
  private[host] def entryPath(path: String): Path =
    val lexical = absolute(path).toAbsolutePath.nn.normalize.nn
    val parent = lexical.getParent
    val name = lexical.getFileName
    if parent != null && name != null && PlatformPath.isLink(lexical) then
      PlatformPath.canonical(parent).resolve(name).nn
    else PlatformPath.canonical(lexical)

  private def absolute(path: String): Path =
    val raw = PlatformPath.parse(path)
    // Canonicalizing a UNC path contacts its server (DNS, SMB) before any permission check,
    // so even a read-only file system inside `Classified.map` could reach the network.
    if PlatformPath.isUnc(raw) && raw.getRoot != cwd.getRoot then
      throw IllegalArgumentException(
        s"UNC path ${ScalaSource.stringLiteral(path)} is not supported outside the working directory's share"
      )
    if raw.isAbsolute then raw else cwd.resolve(raw).nn

  /** `operation` carries its own preposition, so that it reads as a phrase in
    * front of the path (`read '/x'`, `running a command in '/x'`). */
  private def denied(path: Path, operation: String, permission: Perm, hint: String): SecurityException =
    val shown = PlatformPath.portable(path)
    val guidance =
      if permission.locked then
        "The file rule is locked. Permission requests cannot widen access; the user must change the configuration."
      else hint
    SecurityException(
      s"Access denied: $operation '$shown' is not permitted (current permission: ${permission.describe}). $guidance"
    )

  private[host] def requireRead(scope: ScopeId, path: Path, operation: String): Perm =
    requireAccess(scope, path, operation, write = false)

  private[host] def requireWrite(scope: ScopeId, path: Path, operation: String): Perm =
    requireAccess(scope, path, operation, write = true)

  private def requireAccess(scope: ScopeId, path: Path, operation: String, write: Boolean): Perm =
    val permission = policy.effective(scope, path)
    if !(if write then permission.canWrite else permission.canRead) then
      val access = if write then "Access.Write" else "Access.Read"
      // A request names a path that is resolved, so a link is granted through its directory.
      val requested = if PlatformPath.isLink(path) then Option(path.getParent).getOrElse(path) else path
      val shown = ScalaSource.stringLiteral(PlatformPath.portable(requested))
      throw denied(path, operation, permission, s"Use requestFiles($shown, $access, reason) { ... } to ask the user.")
    permission

  private def requireNotClassified(permission: Perm, path: Path, operation: String, alternative: String): Unit =
    if permission.classified then
      throw SecurityException(
        s"Access denied: '${PlatformPath.portable(path)}' is classified; '$operation' would reveal its content. Use $alternative instead."
      )

  /** Require read access and that the content is not classified. `alternative`
    * names what to use instead on a classified path. */
  private[host] def requireReadable(scope: ScopeId, path: Path, operation: String, alternative: String): Perm =
    val permission = requireRead(scope, path, operation)
    requireNotClassified(permission, path, operation, alternative)
    permission

  /** Require write access and that the target is not classified. */
  private[host] def requireWritable(
    scope: ScopeId,
    path: Path,
    operation: String,
    alternative: String = "writeClassified(path, classify(content))"
  ): Perm =
    val permission = requireWrite(scope, path, operation)
    requireNotClassified(permission, path, operation, alternative)
    permission

  private[host] def ensureParent(path: Path): Unit = Option(path.getParent).foreach(Files.createDirectories(_))

  /** Run `body` and report what it changed at `path` through [[HostOutput.fileChanged]]. */
  private[host] def withFileChange[A](path: Path, operation: String)(body: => A): A =
    val before = FileChange.snapshot(path)
    val result = body
    try
      FileChange.between(fromCwd(PlatformPath.portable(path)), operation, before, FileChange.snapshot(path))
        .foreach(output.fileChanged)
    catch case NonFatal(error) => Debug.trace(error)
    result

  private[atc] def writeFile(scope: ScopeId, path: Path, content: String, append: Boolean): Unit =
    requireWritable(scope, path, if append then "append" else "write")
    withFileChange(path, "updated"):
      ensureParent(path)
      if append then
        Files.writeString(path, content, StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
      else Files.writeString(path, content, StandardCharsets.UTF_8)

  private[host] def writeFileBytes(scope: ScopeId, path: Path, content: Array[Byte]): Unit =
    requireWritable(scope, path, "writeBytes")
    withFileChange(path, "updated"):
      ensureParent(path)
      Files.write(path, content)
    ()

  private[host] def writeClassifiedFile(scope: ScopeId, path: Path, content: Try[String]): Unit =
    // Check the target before inspecting the classified computation. Otherwise
    // its success or failure could become an observable bit.
    val permission = requireWrite(scope, path, "writeClassified")
    if !permission.classified then
      throw SecurityException(
        s"Access denied: '${PlatformPath.portable(path)}' is not a classified path; writing classified content there would declassify it."
      )
    ensureParent(path)
    // `exists` works on classified paths, so the target is created before the
    // computation is inspected: neither its failure nor content that cannot be
    // encoded may decide whether the file exists afterwards.
    try
      if !Files.exists(path) then
        Files.createFile(path)
        ()
    catch case NonFatal(_) => ()
    content match
      case Success(value) =>
        // Once the classified computation has been inspected, neither an I/O
        // failure, fatal errors included, nor its message may reach the agent:
        // either would reveal a success/failure bit, and an exception can quote
        // the content.
        if ClassifiedImpl.attempt(Files.writeString(path, value, StandardCharsets.UTF_8)).isFailure then
          classifiedSinkFailed(s"writing '${PlatformPath.portable(path)}'")
      case Failure(_) => classifiedSinkFailed(s"writing '${PlatformPath.portable(path)}'")

  /** Visible children. Policy checks use canonical paths; gitignore checks use the entry. */
  private def visibleEntries(scope: ScopeId, dir: Path): List[VisibleEntry] =
    Using.resource(Files.list(dir).nn): stream =>
      stream.iterator.nn.asScala.toList.sortBy(_.getFileName.toString).flatMap: entry =>
        try
          // Windows junctions and other reparse points are not reported as symbolic links,
          // but their NOFOLLOW attributes are `isOther`. Keep every final reparse point out
          // of recursive traversal and evaluate policy on what it actually reaches.
          val own = entry.toAbsolutePath.nn.normalize.nn
          val reached = PlatformPath.canonical(own)
          val attributes = Files.readAttributes(entry, classOf[BasicFileAttributes], LinkOption.NOFOLLOW_LINKS).nn
          val isLink = attributes.isSymbolicLink || attributes.isOther
          if gitIgnore.ignores(entry) then None
          else
            val permission = policy.effective(scope, reached)
            Option.when(permission.canRead):
              // A link is listed under its own path, so that deleting it removes the link; its
              // own attributes do not describe what it reaches.
              if isLink then VisibleEntry(own, permission, isLink, Files.isDirectory(reached))
              else VisibleEntry(reached, permission, isLink, attributes.isDirectory)
        catch
          case NonFatal(_) => None

  private[host] def visibleChildren(scope: ScopeId, dir: Path): List[Path] = visibleEntries(scope, dir).map(_.path)

  /** Visible descendants in pre-order. Classified trees require an explicit
    * classified traversal, and symlinked directories are never followed. */
  private[host] def walkPaths(scope: ScopeId, dir: Path, intoClassified: Boolean): List[Path] =
    iteratePaths(scope, dir, intoClassified).map(_.path).toList

  private[host] def iteratePaths(scope: ScopeId, dir: Path, intoClassified: Boolean): Iterator[VisibleEntry] =
    def visit(current: Path): Iterator[VisibleEntry] =
      visibleEntries(scope, current).iterator.flatMap: entry =>
        val descend = !entry.isLink && entry.isDirectory && (intoClassified || !entry.permission.classified)
        Iterator.single(entry) ++ (if descend then visit(entry.path) else Iterator.empty)
    visit(dir)

  private lazy val canonicalCwd: Path = canonical(".")

  /** `absolute` relative to the working directory when inside it, with `/` separators. */
  private[host] def fromCwd(absolute: String): String =
    val path = Paths.get(absolute).nn
    if path == canonicalCwd then "."
    else if path.startsWith(canonicalCwd) then PlatformPath.portable(canonicalCwd.relativize(path).nn)
    else PlatformPath.portable(path)

/** A listed directory entry: its canonical path, or a link's own path under its canonical
  * parent; the permission of what it reaches; whether the entry itself is a symlink or another
  * reparse point; and whether what it reaches is a directory. */
private[host] final case class VisibleEntry(path: Path, permission: Perm, isLink: Boolean, isDirectory: Boolean)
