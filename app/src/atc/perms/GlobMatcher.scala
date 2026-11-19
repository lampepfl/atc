package atc.perms

import atc.platform.Platform

import java.net.{InetAddress, UnknownHostException}
import java.util.Locale
import scala.util.Try

/** Glob matching for command lines and host names: `*` matches any sequence of characters,
  * everything else is literal. Adapted from TACIT. */
object GlobMatcher:
  private val WindowsExecutableSuffixes = List(".exe", ".com", ".cmd", ".bat")

  /** Full-string glob matching without regex compilation. A `*` matches any characters, line
    * terminators included: a rendered argument may hold U+2028, and a deny pattern must still
    * match past it. */
  def matches(value: String, pattern: String): Boolean = matches(value, pattern, 0, 0)

  /** [[matches]], comparing the characters of `value` in `[foldFrom, foldUntil)` without case. */
  private def matches(value: String, pattern: String, foldFrom: Int, foldUntil: Int): Boolean =
    // Upper case first: a file system that ignores case takes `ſ` for `s`, and NTFS `ı` for
    // `i`, which are lower case already.
    def fold(c: Char) = Character.toLowerCase(Character.toUpperCase(c))
    def same(p: Char, index: Int) =
      val v = value.charAt(index)
      p == v || (index >= foldFrom && index < foldUntil && fold(p) == fold(v))
    var valueIndex = 0
    var patternIndex = 0
    var starIndex = -1
    var retryValueIndex = -1

    while valueIndex < value.length do
      if patternIndex < pattern.length && pattern.charAt(patternIndex) == '*' then
        starIndex = patternIndex
        patternIndex += 1
        retryValueIndex = valueIndex
      else if patternIndex < pattern.length && same(pattern.charAt(patternIndex), valueIndex) then
        patternIndex += 1
        valueIndex += 1
      else if starIndex >= 0 then
        retryValueIndex += 1
        valueIndex = retryValueIndex
        patternIndex = starIndex + 1
      else return false

    while patternIndex < pattern.length && pattern.charAt(patternIndex) == '*' do patternIndex += 1
    patternIndex == pattern.length

  /** Command-line matching. A pattern containing `*` matches as a glob. One without `*` matches
    * the command line it equals and any command line it is a word prefix of: `"ls"` permits
    * `ls -la` and `"git status"` permits `git status --short`, while `"git diff*"` also permits
    * `git difftool`. */
  def matchesCommand(commandLine: String, pattern: String): Boolean =
    matchesWhole(normalizeCommand(commandLine.trim), normalizeCommand(pattern.trim))

  /** [[matchesCommand]] for an allow pattern. A line and a pattern that both begin with
    * `NAME=value` match in two parts: the assignment against the pattern's assignment, then the
    * command against the rest of the pattern, which matches any command when empty. The agent
    * chooses the value, so a `*` in it must not reach the command: `PATH=* npm *` does not permit
    * `PATH="x npm y" make`. Deny patterns keep matching the whole line, so one whose `*` spans the
    * value and the command (`FOO=*curl*`) still refuses `FOO=1 curl x`. */
  def permitsCommand(commandLine: String, pattern: String): Boolean =
    permitsNormalized(normalizeCommand(commandLine.trim), normalizeCommand(pattern.trim))

  private def permitsNormalized(command: String, p: String): Boolean =
    (assignmentEnd(command), assignmentEnd(p)) match
      case (Some(commandEnd), Some(patternEnd)) =>
        val assignment = command.substring(0, commandEnd)
        val assignmentPattern = p.substring(0, patternEnd)
        val assigns =
          if assignmentPattern.contains('*') then matches(assignment, assignmentPattern)
          else assignment == assignmentPattern
        val rest = p.substring(patternEnd).trim
        assigns && (rest.isEmpty || permitsNormalized(command.substring(commandEnd).trim, rest))
      case _ => matchesWhole(command, p)

  private def matchesWhole(command: String, p: String): Boolean =
    val (from, until) = programWord(command)
    if p.isEmpty then false
    else if p.contains('*') then matches(command, p, from, until)
    else matches(command, p, from, until) || matches(command, p + " *", from, until)

  /** Where the program word of a command line lies, after a leading `NAME=value`. Where program
    * lookup ignores case, on Windows and on macOS as APFS is formatted, a pattern is compared
    * with it without case: `BASH` runs `bash` there, so a deny on `bash` must refuse it.
    * Arguments keep their case. Empty where names that differ in case are different programs. */
  private def programWord(command: String): (Int, Int) =
    if !Platform.caseInsensitivePaths then (0, 0)
    else
      val start = assignmentEnd(command).fold(0)(end => (end + 1).min(command.length))
      (start, wordEnd(command, start))

  /** Where the word of `value` that begins at `start` ends: after its closing quote, or at the
    * next space. */
  private def wordEnd(value: String, start: Int): Int =
    val end = if value.startsWith("\"", start) then value.indexOf('"', start + 1) + 1 else value.indexOf(' ', start)
    if end <= 0 then value.length else end

  /** Windows spellings of one command. A variable's name ignores case, and in the program word
    * `\` separates like `/` and the standard executable and script suffixes are details, so a
    * deny on `git push*` cannot be bypassed as `GIT.EXE push`. Explicit paths stay explicit and
    * must still be granted as such. The case of the program word is [[programWord]]'s matter. */
  private def normalizeCommand(value: String): String =
    if !Platform.isWindows then value
    else
      assignmentEnd(value) match
        case Some(end) =>
          // `NAME=value command`, how a variable is permitted: the name ignores case,
          // the value keeps it, and the command after it is normalized as usual.
          val equals = value.indexOf('=')
          val assignment = value.substring(0, equals).toUpperCase(Locale.ROOT) + value.substring(equals, end)
          if end >= value.length then assignment else assignment + " " + normalizeCommand(value.substring(end + 1))
        case None => normalizeProgram(value)

  /** Whether a command line or pattern begins with a `NAME=` word: how a variable a command
    * runs with is matched. */
  def startsWithAssignment(value: String): Boolean = assignmentEnd(value.trim).isDefined

  /** Where a leading `NAME=value` word ends (at the space after it, or the end): the
    * value as `CommandLine.assignment` renders it, plain or in `"..."` with `\` escapes.
    * A program word never has that shape; the command parser refuses it. */
  private def assignmentEnd(value: String): Option[Int] =
    val equals = value.indexOf('=')
    if equals <= 0 || !value.substring(0, equals).matches("[A-Za-z_][A-Za-z0-9_]*") then None
    else
      var index = equals + 1
      var quoted = false
      while index < value.length && (quoted || value.charAt(index) != ' ') do
        value.charAt(index) match
          case '\\' if quoted => index += 1
          case '"' => quoted = !quoted
          case _ => ()
        index += 1
      Some(index.min(value.length))

  private def normalizeProgram(value: String): String =
    val boundary = wordEnd(value, 0)
    val command = value.substring(0, boundary).replace('\\', '/')
    val suffix = value.substring(boundary)
    val plainName = !command.exists(c => c == '/' || c == '*' || c == '?')
    val executableSuffix =
      if !plainName then None
      else WindowsExecutableSuffixes.find(s => command.regionMatches(true, command.length - s.length, s, 0, s.length))
    executableSuffix.fold(command)(extension => command.dropRight(extension.length)) + suffix

  /** Normalize a concrete host or host pattern without resolving DNS names. Exact numeric literals
    * are canonicalized; globs keep their shape. Both policy patterns and request hosts pass
    * through this, so an exact `::1` rule also covers `[0:0:0:0:0:0:0:1]`. */
  def normalizeHost(value: String): String =
    val normalized = value.trim.stripSuffix(".").toLowerCase(Locale.ROOT)
    if normalized.contains('*') then normalized
    else
      val (bare, bracketed) =
        if normalized.startsWith("[") && normalized.endsWith("]") then
          (normalized.substring(1, normalized.length - 1), true)
        else (normalized, false)
      literalIpAddress(bare)
        .orElse(if bare.contains(':') && (bracketed || looksLikeIpv6(bare)) then ipv6Literal(bare) else None)
        .getOrElse(bare)

  private def looksLikeIpv6(value: String): Boolean =
    value.contains(':') && value.forall: char =>
      char == ':' || char == '.' ||
        (char >= '0' && char <= '9') ||
        (char >= 'a' && char <= 'f')

  /** The canonical form of an IPv6 literal. In brackets and with a `:`, `getByName` parses the
    * value as a literal or rejects it; it never falls back to a DNS lookup. */
  private def ipv6Literal(value: String): Option[String] =
    try Some(InetAddress.getByName(s"[$value]").nn.getHostAddress.nn)
    catch case _: UnknownHostException => None

  /** Convert a numeric IPv4 literal with one to four decimal parts into canonical dotted-quad form
    * without a DNS lookup. */
  private def literalIpAddress(value: String): Option[String] =
    val parts = value.split("\\.", -1).toList
    // Decimal digits only (`toLongOption` would also take a sign), and no overflow.
    val numbers = parts.filter(part => part.nonEmpty && part.forall(char => char >= '0' && char <= '9'))
      .flatMap(_.toLongOption)

    if parts.isEmpty || parts.lengthIs > 4 then None
    else
      for
        values <- Option.when(numbers.lengthIs == parts.length)(numbers)
        lastMax = 1L << (8 * (5 - values.length))
        if values.init.forall(_ <= 255) && values.last < lastMax
        address = values.init.zipWithIndex.foldLeft(values.last) { case (current, (part, index)) =>
          current | (part << (8 * (3 - index)))
        }
        bytes = Array.tabulate(4)(index => ((address >> (8 * (3 - index))) & 0xff).toByte)
        canonical <- Try(InetAddress.getByAddress(bytes).nn.getHostAddress.nn).toOption
      yield canonical

  /** Host matching: normalized plain glob on the host name (case-insensitive). */
  def matchesHost(host: String, pattern: String): Boolean =
    matches(normalizeHost(host), normalizeHost(pattern))
