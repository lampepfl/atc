package atc.perms

import atc.platform.{PathGlob, Platform, PlatformPath}

import java.nio.file.{Path, Paths}
import java.util.regex.Pattern

/** A path pattern from the configuration, in the style of gitignore (as in TACIT):
  *
  *  - no `/` in the pattern: matches any path component by glob
  *    (`.env`, `.env.*`, `node_modules`, `*.pem`)
  *  - relative pattern with `/`: relative to `base` (the working directory),
  *    supports `*`, `**`, `?`, `[…]` and `{a,b}` (`src/main`, `build/[a-z]*`)
  *  - absolute pattern (or starting with `~`): matched against the absolute
  *    path (`/etc`, `~/notes`, `~/proj/x/build`)
  *  - `.` means `base` itself
  *  - a trailing `/` is stripped
  *
  * A pattern matches a path if it matches the path itself or any ancestor,
  * so a rule on a directory covers its whole subtree. Names compare the way the
  * file system does, case-insensitively on Windows and macOS: canonicalization
  * corrects the case of existing names only, so a rule must also cover a new name
  * spelled in another case.
  */
final class PathPattern private (val raw: String, private val kind: PathPattern.Kind):
  import PathPattern.*

  def matches(p: Path): Boolean = kind match
    case Kind.Component(glob) => (0 until p.getNameCount).exists(i => glob.matcher(p.getName(i).toString).matches())
    case Kind.Anchored(root, matchers) =>
      within(p, root) && {
        val relative =
          if p.getNameCount == root.getNameCount then ""
          else PlatformPath.portable(p.subpath(root.getNameCount, p.getNameCount))
        matchers.exists(_.matcher(relative).matches())
      }
    case Kind.Exact(path) => within(p, path)

  /** [[matches]] for a path whose parent this pattern does not match. A component glob can then
    * match only the path's last name, since every other name is also the parent's. */
  private[perms] def matchesChild(p: Path): Boolean = kind match
    case Kind.Component(glob) => Option(p.getFileName).exists(name => glob.matcher(name.toString).matches())
    case _ => matches(p)

  override def toString: String = raw

object PathPattern:
  private enum Kind:
    /** Matched against each name of the path. */
    case Component(glob: Pattern)
    /** `root` is an absolute, real path; matchers are applied to the path relative to it. */
    case Anchored(root: Path, matchers: List[Pattern])
    case Exact(path: Path)

  /** Whether `p` is `root` or below it, comparing names with the file system's case semantics. */
  private def within(p: Path, root: Path): Boolean =
    p.getRoot == root.getRoot && p.getNameCount >= root.getNameCount &&
      (0 until root.getNameCount).forall(i => Platform.samePathName(p.getName(i).toString, root.getName(i).toString))

  private val globChars = "*?[{"

  /** Parse `pattern`, resolving a relative one against `base`. Throws `IllegalArgumentException`
    * for a malformed glob or a path Windows cannot name safely. */
  def apply(pattern: String, base: Path): PathPattern =
    val expanded = PlatformPath.expandHome(pattern.trim)
    val trimmed = if PlatformPath.isRoot(expanded) then expanded else PlatformPath.stripTrailingSeparators(expanded)
    val path = if trimmed.isEmpty then "." else trimmed
    requireValid(PlatformPath.driveRelativeValidationError(path))
    if path == "." then new PathPattern(pattern, Kind.Exact(PlatformPath.canonical(base)))
    else if !PlatformPath.hasSeparator(path) then
      if !globChars.exists(path.contains(_)) then requireValid(PlatformPath.validationError(path))
      new PathPattern(pattern, Kind.Component(PathGlob.pattern(path)))
    else
      // Windows refuses `*`, `?` and several other glob characters in a Path,
      // so never hand the glob-bearing suffix to Paths.get. Split it as text,
      // parse only the literal prefix, and match the remainder in a stable
      // slash-separated form on every platform.
      val normalized = PlatformPath.slashSeparated(path)
      val (prefix, rest) = splitGlob(normalized)
      requireValid(PlatformPath.validationError(prefix))
      val literal = Paths.get(PlatformPath.native(prefix)).nn
      val root = if literal.isAbsolute then literal else base.resolve(literal).nn
      if rest.isEmpty then new PathPattern(pattern, Kind.Exact(PlatformPath.canonical(root)))
      else new PathPattern(pattern, Kind.Anchored(PlatformPath.canonical(root), globOrDescendantMatchers(rest)))

  private def requireValid(error: Option[String]): Unit =
    error.foreach(reason => throw IllegalArgumentException(s"invalid Windows path: $reason"))

  /** Split a slash-normalized pattern before the first glob-bearing component.
    * The returned prefix contains no characters Windows rejects in a Path. */
  private def splitGlob(value: String): (String, String) =
    value.indexWhere(globChars.contains) match
      case -1 => (value, "")
      case firstGlob =>
        val boundary = value.lastIndexOf('/', firstGlob)
        if boundary < 0 then ("", value)
        else (value.substring(0, boundary + 1), value.substring(boundary + 1))

  /** Matchers for the glob and for everything below what it matches. The syntax is slash-based on
    * every platform; matching is case-insensitive where the file system is. */
  private def globOrDescendantMatchers(glob: String): List[Pattern] =
    List(PathGlob.pattern(glob), PathGlob.pattern(s"$glob/**"))
