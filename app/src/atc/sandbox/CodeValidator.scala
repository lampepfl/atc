package atc.sandbox

import scala.collection.mutable
import scala.util.matching.Regex

/** A violation found by the code validator. */
final case class Violation(ruleId: String, description: String, lineNumber: Int, snippet: String)

/** Lexical checks on agent code before it reaches the compiler. The API rules give early
  * diagnostics; they are not a safety boundary, which safe mode and capture checking
  * provide. The catch rules, with [[CatchArms]], are what keeps agent code from catching
  * fatal errors and the `ThreadDeath` stop signal. Each check is a linear scan without
  * parsing, with false positives and false negatives. Adapted from TACIT (Apache-2.0). */
object CodeValidator:

  /** A per-line rule. One that `keepComments` is matched against the physical lines of
    * the code with only string literals blanked, since `//> using` is a comment. */
  private final case class Forbidden(id: String, regex: Regex, description: String, keepComments: Boolean = false)

  /** Throwable and the fatal error types. */
  private val FatalClasses =
    List("Throwable", "ControlThrowable", "Error", "VirtualMachineError", "StackOverflowError", "OutOfMemoryError")

  /** Supertypes of Throwable: a catch of one catches every throwable. */
  private val TopTypes = List("Any", "AnyRef", "Object", "Serializable", "Matchable")

  /** The interruption signals: the thread interrupt and the sandbox's stop signal. */
  private val StopSignals = List("InterruptedException", "ThreadDeath")

  /** Every type name whose catch could swallow a fatal error or a stop signal. Renaming one on
    * import (`import java.lang.Throwable as Fatal`) would defeat the catch rules, in every mode. */
  private val FatalTypeNames = FatalClasses ++ TopTypes ++ StopSignals

  /** A regex alternative of `names`, each optionally qualified by a package or object path. */
  private def typeNameAlternatives(names: List[String]): String = raw"(?:[\w.]+\.)?(?:${names.mkString("|")})\b"

  /** The `catch-fatal` rule, shared by the per-line scan and the catch-arm
    * scanner (which reports an ascription the per-line regex cannot see). */
  private val CatchFatalRe: Regex =
    (raw"\bcase\s+(?!class\b|object\b)[^=]*:(?:(?!=>|\bif\b)[^=])*\b" +
      typeNameAlternatives(FatalClasses ++ TopTypes)).r
  private val CatchFatalDescription: String =
    "Catching Throwable/Error/a fatal error is forbidden; catch a specific non-fatal type instead, e.g. case _: Exception (or a RuntimeException subtype)"

  private val forbidden: List[Forbidden] = List(
    // File IO bypass
    Forbidden("file-io-java", raw"java\.io\b".r, "Direct java.io access is forbidden; use the file API"),
    Forbidden("file-io-nio", raw"java\.nio\b".r, "Direct java.nio access is forbidden; use the file API"),
    Forbidden("file-io-scala", raw"scala\.io\b".r, "Direct scala.io access is forbidden; use the file API"),
    // Process bypass
    Forbidden("proc-builder", raw"ProcessBuilder".r, "ProcessBuilder is forbidden; use exec"),
    Forbidden("proc-runtime", raw"Runtime\.getRuntime".r, "Runtime.getRuntime is forbidden; use exec"),
    Forbidden("proc-scala", raw"\bsys\.process\b".r, "scala.sys.process is forbidden; use exec"),
    // Network bypass
    Forbidden("net-java", raw"java\.net\b".r, "Direct java.net access is forbidden; use the http API"),
    Forbidden("net-javax", raw"javax\.net\b".r, "Direct javax.net access is forbidden; use the http API"),
    Forbidden("net-http-client", raw"HttpClient".r, "HttpClient is forbidden; use the http API"),
    Forbidden("net-http-conn", raw"HttpURLConnection".r, "HttpURLConnection is forbidden; use the http API"),
    // Cast escape
    Forbidden("cast-escape", raw"\.asInstanceOf\s*\[".r, ".asInstanceOf is forbidden"),
    // Capture-checking escape hatches
    Forbidden("cc-unsafe-caps", raw"caps\.unsafe".r, "caps.unsafe explicitly escapes capture checking"),
    Forbidden("cc-unsafe-pure", raw"unsafeAssumePure".r, "unsafeAssumePure explicitly escapes capture checking"),
    Forbidden("cc-unsafe-assume", raw"unsafeAssume\w*".r, "caps.unsafe.* explicitly escapes capture checking"),
    Forbidden("cc-assume-safe", raw"assumeSafe".r, "@assumeSafe may only be used by the library"),
    // Sandbox internals
    Forbidden(
      "atc-host",
      raw"atc\.(host|agent|sandbox|perms|config|llm|ui)\b".r,
      "application internals are not accessible"
    ),
    Forbidden(
      "atc-runtime",
      raw"\b(?:atc\.lib\.)?Runtime\b".r,
      "Runtime.current/rootIO/rootUser/install/fileSystem/readOnlyFileSystem/processes/network are internal to the sandbox"
    ),
    // Reflection
    Forbidden("reflect-method", raw"getDeclaredMethod".r, "Reflective access is forbidden"),
    Forbidden("reflect-field", raw"getDeclaredField".r, "Reflective access is forbidden"),
    Forbidden("reflect-ctor", raw"getDeclaredConstructor".r, "Reflective access is forbidden"),
    Forbidden("reflect-accessible", raw"setAccessible".r, "Reflective access is forbidden"),
    Forbidden("reflect-java", raw"java\.lang\.reflect\b".r, "java.lang.reflect is forbidden"),
    Forbidden("reflect-scala", raw"scala\.reflect\.runtime".r, "scala.reflect.runtime is forbidden"),
    Forbidden("reflect-forname", raw"Class\.forName".r, "Class.forName is forbidden"),
    // No leading dot: backquoted `` x.`getClass` `` must be caught too (safe mode
    // allows getClass, which leaks host implementation class names). Strings and
    // comments are stripped before matching, so text false positives are impossible.
    Forbidden("reflect-getclass", raw"\bgetClass\b".r, "getClass is forbidden"),
    // JVM internals
    Forbidden("jvm-jdk-internal", raw"jdk\.internal\b".r, "jdk.internal access is forbidden"),
    Forbidden("jvm-sun", raw"\bsun\.\w+".r, "sun.* access is forbidden"),
    Forbidden("jvm-com-sun", raw"com\.sun\.\w+".r, "com.sun.* access is forbidden"),
    Forbidden("jvm-invoke", raw"java\.lang\.invoke\b".r, "java.lang.invoke is forbidden"),
    // Output bypass
    Forbidden("io-system-out", raw"System\.out\b".r, "System.out is forbidden; use println"),
    Forbidden("io-system-err", raw"System\.err\b".r, "System.err is forbidden; use println"),
    Forbidden("io-system-in", raw"System\.in\b".r, "System.in is forbidden"),
    Forbidden("io-console", raw"\bConsole\b".r, "scala.Console is forbidden; use println"),
    Forbidden("io-predef-print", raw"Predef\.print".r, "Predef.println/print is forbidden; use println"),
    // System control
    Forbidden("sys-exit", raw"System\.exit".r, "System.exit is forbidden"),
    Forbidden("sys-setprop", raw"System\.setProperty".r, "System.setProperty is forbidden"),
    Forbidden("sys-getenv", raw"System\.getenv".r, "System.getenv is forbidden"),
    Forbidden("sys-getprop", raw"System\.getProperty".r, "System.getProperty is forbidden"),
    Forbidden("sys-load", raw"System\.load\w*".r, "System.load is forbidden"),
    Forbidden(
      "sys-system-import",
      raw"\bimport\s+(?:java\.lang\.)?System\b".r,
      "Importing System or its members is forbidden; call only the permitted time/line-separator methods directly"
    ),
    Forbidden(
      "sys-scala",
      raw"\bsys\.(exit|env|props|runtime|allThreads|addShutdownHook)\b".r,
      "scala.sys.* (exit/env/props/runtime/...) is forbidden"
    ),
    Forbidden("sys-thread", raw"\bnew\s+Thread\b".r, "Creating threads is forbidden"),
    Forbidden("sys-thread2", raw"\bThread\s*\(".r, "Creating threads is forbidden"),
    // Catching fatal throwables. The sandbox's ThreadDeath stop signal is an Error, so a
    // catch of Throwable, Error or another fatal type could let a loop swallow a timeout or
    // an interrupt, and a fatal error must end the evaluation. Secret-dependent failures do
    // not reach these catches: `Classified.map` keeps everything its callback throws inside
    // the classified value. A typed catch of Throwable/Error/a fatal type is rejected here; a
    // bare catch-all is caught by the cross-line `catch-all` rule below. A typed catch of a
    // non-fatal type (`case _: Exception`, a RuntimeException subtype, ...) stays allowed.
    // The fatal type may appear anywhere in the ascription, so a parenthesised
    // (`case _: (Throwable)`) or union (`case _: (RuntimeException | Throwable)`)
    // type does not slip past. The scan of the ascription stops before the arm
    // arrow `=>` and before an `if` guard, so a guard that merely mentions a
    // fatal type (`case _: Foo if Throwable.check() =>`) is not a false positive.
    Forbidden("catch-fatal", CatchFatalRe, CatchFatalDescription),
    // An erased type parameter bounded by a fatal type would defeat `catch-fatal`:
    // `def g[T <: Throwable] = try ... catch case _: T` erases to a catch of the
    // bound. `Any`/`AnyRef` are left out: `[T <: AnyRef]` is a common, legitimate
    // bound, and this rule targets an explicit fatal upper bound.
    Forbidden(
      "catch-fatal-bound",
      (raw"<:\s*" + typeNameAlternatives(FatalClasses ++ StopSignals)).r,
      "A type parameter bounded by Throwable/Error/a fatal type is forbidden; `case _: T` would then catch fatal throwables"
    ),
    Forbidden(
      "throwable-interrupted",
      raw"\bInterruptedException\b".r,
      "InterruptedException may not be used in agent code (throwing or catching it can defeat the interrupt/timeout)"
    ),
    // A type alias for a fatal type would defeat `catch-fatal`: `type T = Throwable`
    // followed by `catch case _: T =>` names no forbidden type textually. The fatal
    // type must be a top-level constituent of the right-hand side (the alias target
    // itself or a `|`/`&` member); a fatal type nested inside type arguments is fine
    // (`type M = Map[String, AnyRef]` aliases Map, not AnyRef), so the RHS scan does
    // not cross a `[`.
    Forbidden(
      "catch-fatal-alias",
      (raw"\btype\s+\w+(?:\[[^\]\n]*\])?\s*=\s*(?:[^\[=\n|&]*[|&]\s*)*" + typeNameAlternatives(FatalTypeNames)).r,
      "Aliasing Throwable/Error/a fatal error type is forbidden; it would defeat the ban on catching fatal throwables"
    ),
    Forbidden(
      "throwable-threaddeath",
      raw"\bThreadDeath\b".r,
      "ThreadDeath is the sandbox stop signal and may not be used in agent code"
    ),
    // Library catch helpers catch inside trusted code, where no catch rule can see them:
    // `Exception.allCatch` catches every throwable, the stop signal included. Safe mode
    // rejects them; without it this rule covers a qualified use, a selector and a wildcard import.
    Forbidden(
      "catch-helper",
      raw"\bcontrol\.(?:(?:\{[^}]*\b)?(?:Breaks|Exception|NonLocalReturns)\b|\*|_\b|\{\s*[*_]\s*\})".r,
      "scala.util.control.Exception, Breaks and NonLocalReturns are forbidden: they catch fatal errors, the stop signal or control throwables inside the library; use try/catch with a typed arm"
    ),
    // The REPL turns some warnings into errors (a non-local `return`); `@nowarn` would silence them.
    Forbidden(
      "nowarn",
      raw"\bnowarn\b".r,
      "@nowarn is forbidden: it would silence warnings the sandbox treats as errors, such as a non-local return"
    ),
    // Directives
    Forbidden("directive-using", raw"//>\s*using".r, "//> using directives are forbidden", keepComments = true),
    Forbidden("directive-import", """import\s+\$""".r, "import $ directives are forbidden", keepComments = true),
    // Class loading / compiler
    Forbidden("classloader", raw"ClassLoader".r, "ClassLoader access is forbidden"),
    Forbidden("dotty-tools", raw"dotty\.tools\b".r, "dotty.tools access is forbidden"),
    Forbidden("scala-tools", raw"scala\.tools\b".r, "scala.tools access is forbidden"),
    Forbidden("scala-quoted", raw"scala\.quoted\b".r, "scala.quoted access is forbidden"),
    Forbidden(
      "language-import",
      raw"import\s+(scala\.)?language\.experimental\b".r,
      "language imports are managed by the sandbox",
      keepComments = true
    ),
  )

  private[sandbox] inline def isIdentChar(c: Char): Boolean = Character.isLetterOrDigit(c) || c == '_'
  private[sandbox] inline def isIdentStart(c: Char): Boolean = Character.isLetter(c) || c == '_'

  private def charLiteralLength(code: String, i: Int, len: Int): Int =
    if code.charAt(i) != '\'' then 0
    else if i + 1 < len && code.charAt(i + 1) == '\\' then
      if i + 2 < len && code.charAt(i + 2) == 'u' then
        if i + 7 < len && code.charAt(i + 7) == '\'' then 8 else 0
      else if i + 3 < len && code.charAt(i + 3) == '\'' then 4
      else 0
    else if i + 2 < len && code.charAt(i + 2) == '\'' then 3
    else 0

  /** Blank string literals and comments (keeping `${...}` interpolations, which
    * are code), preserving newlines and character offsets. */
  def stripLiteralsAndComments(code: String): String = strip(code, blankComments = true)

  /** Blank string literals only; comments are kept (directives live there). */
  def stripStringLiteralsOnly(code: String): String = strip(code, blankComments = false)

  private def strip(code: String, blankComments: Boolean): String =
    val out = StringBuilder(code.length)
    val len = code.length
    final class Frame(val isString: Boolean, val triple: Boolean, val interp: Boolean, val fromInterp: Boolean):
      var brace: Int = 0
    val stack = mutable.Stack[Frame](Frame(false, false, false, false))

    inline def emit(c: Char): Unit = out.append(c)
    inline def blank(c: Char): Unit = out.append(if c == '\n' then '\n' else ' ')
    /** A character of a comment: blanked or kept, according to `blankComments`. */
    inline def comment(c: Char): Unit = if blankComments then blank(c) else emit(c)
    /** Whether the `"` at `k` starts `"""`. */
    def tripleQuoteAt(k: Int): Boolean = k + 2 < len && code.charAt(k + 1) == '"' && code.charAt(k + 2) == '"'

    var i = 0
    while i < len do
      val f = stack.top
      val c = code.charAt(i)
      if f.isString then
        if !f.triple && c == '\\' && i + 1 < len then
          blank(c)
          blank(code.charAt(i + 1))
          i += 2
        else if f.interp && c == '$' && i + 1 < len && code.charAt(i + 1) == '{' then
          emit('$')
          emit('{')
          i += 2
          stack.push(Frame(false, false, false, true))
        else if f.interp && c == '$' && i + 1 < len && isIdentStart(code.charAt(i + 1)) then
          emit('$')
          i += 1
          while i < len && isIdentChar(code.charAt(i)) do
            emit(code.charAt(i))
            i += 1
        else if c == '"' && (!f.triple || tripleQuoteAt(i)) then
          val width = if f.triple then 3 else 1
          for _ <- 0 until width do blank('"')
          i += width
          stack.pop()
        else
          blank(c)
          i += 1
      else
        val charLiteral = if c == '\'' then charLiteralLength(code, i, len) else 0
        if charLiteral > 0 then
          val end = i + charLiteral
          while i < end do
            blank(code.charAt(i))
            i += 1
        else if c == '"' then
          val triple = tripleQuoteAt(i)
          val interp = i > 0 && isIdentChar(code.charAt(i - 1))
          val width = if triple then 3 else 1
          for _ <- 0 until width do blank('"')
          i += width
          stack.push(Frame(true, triple, interp, false))
        else if c == '/' && i + 1 < len && code.charAt(i + 1) == '/' then
          while i < len && code.charAt(i) != '\n' do
            comment(code.charAt(i))
            i += 1
        else if c == '/' && i + 1 < len && code.charAt(i + 1) == '*' then
          comment('/')
          comment('*')
          i += 2
          var depth = 1 // Scala block comments nest
          while i < len && depth > 0 do
            if code.startsWith("/*", i) || code.startsWith("*/", i) then
              depth += (if code.charAt(i) == '/' then 1 else -1)
              comment(code.charAt(i))
              comment(code.charAt(i + 1))
              i += 2
            else
              comment(code.charAt(i))
              i += 1
        else if c == '{' then
          emit('{')
          f.brace += 1
          i += 1
        else if c == '}' then
          emit('}')
          if f.fromInterp && f.brace == 0 then stack.pop()
          else if f.brace > 0 then f.brace -= 1
          i += 1
        else
          emit(c)
          i += 1
    out.toString

  private val CatchAllDescription: String =
    "A bare catch-all also catches fatal errors and the sandbox stop signal; catch a specific non-fatal type instead, e.g. case _: Exception"

  private val FatalTypeRe = (raw"\b" + typeNameAlternatives(FatalTypeNames)).r
  private val FatalImportAliasDescription: String =
    "Renaming Throwable/Error/a fatal type on import is forbidden; it would defeat the ban on catching fatal throwables"
  private val CatchExtractorDescription: String =
    "A catch arm must be a typed pattern such as case e: IllegalStateException; an extractor or identifier pattern (case NonFatal(e), case Handler()) runs code that can match fatal errors and the sandbox stop signal"
  private val CatchHandlerDescription: String =
    "The handler of `catch` must be written as `case` arms; a handler value (a PartialFunction) cannot be checked for catching fatal errors"
  private val TypeParamCatchDescription: String =
    "Catching a type parameter or abstract type member (`case _: T`) is forbidden: it erases to a catch of its bound and so catches fatal errors; catch a specific non-fatal type instead"

  /** `kw` at `i` as a whole word. */
  private[sandbox] def keywordAt(code: String, i: Int, kw: String): Boolean =
    code.regionMatches(i, kw, 0, kw.length) &&
      (i == 0 || !isIdentChar(code.charAt(i - 1))) &&
      (i + kw.length >= code.length || !isIdentChar(code.charAt(i + kw.length)))

  private val ImportAliasDescription: String =
    "Import aliases are forbidden when safe mode is off because they can hide restricted packages, classes, and methods from validation"

  /** Offsets of `as` / `=>` aliases inside import statements, and separately those that
    * rename a fatal type. Imports may have selectors over several lines, so a per-line
    * regex is not sufficient. */
  private final case class ImportAliases(all: List[Int], fatal: List[Int])
  private def importAliasOffsets(code: String): ImportAliases =
    val hits = mutable.ListBuffer[Int]()
    val fatal = mutable.ListBuffer[Int]()
    val len = code.length
    var i = 0
    while i < len do
      if keywordAt(code, i, "import") then
        var k = i + "import".length
        var braces = 0
        var brackets = 0
        var parens = 0
        var stop = false
        var previousName = "" // the selector an `as`/`=>` renames
        def alias(at: Int): Unit =
          hits += at
          if FatalTypeNames.contains(previousName) then fatal += at
        while k < len && !stop do
          code.charAt(k) match
            case '{' => braces += 1; k += 1
            case '}' => braces = math.max(0, braces - 1); k += 1
            case '[' => brackets += 1; k += 1
            case ']' => brackets = math.max(0, brackets - 1); k += 1
            case '(' => parens += 1; k += 1
            case ')' => parens = math.max(0, parens - 1); k += 1
            case '=' if k + 1 < len && code.charAt(k + 1) == '>' => alias(k); k += 2
            case c if isIdentStart(c) =>
              val start = k
              while k < len && isIdentChar(code.charAt(k)) do k += 1
              if keywordAt(code, start, "as") then alias(start) else previousName = code.substring(start, k).nn
            case ';' if braces == 0 && brackets == 0 && parens == 0 => stop = true; k += 1
            case '\n' if braces == 0 && brackets == 0 && parens == 0 =>
              var before = k - 1
              while before >= 0 && (code.charAt(before) == ' ' || code.charAt(before) == '\t') do before -= 1
              var after = k + 1
              while after < len && (code.charAt(after) == ' ' || code.charAt(after) == '\t') do after += 1
              val continues = (before >= 0 && (code.charAt(before) == '.' || code.charAt(before) == ',')) ||
                (after < len && code.charAt(after) == '.')
              if !continues then stop = true
              k += 1
            case _ => k += 1
        i = k
      else i += 1
    ImportAliases(hits.toList, fatal.toList)

  private val dotWhitespace = raw"\s*\.\s*".r
  private def squeezeDots(line: String): String = dotWhitespace.replaceAllIn(line, ".")

  /** Join physical lines connected by member-access dots so `java.\n io` is
    * seen as `java.io`. Returns `(line, startIndex)`. */
  private def logicalLines(strippedLines: Array[String]): List[(String, Int)] =
    val result = mutable.ListBuffer[(String, Int)]()
    var i = 0
    while i < strippedLines.length do
      val start = i
      val sb = StringBuilder(strippedLines(i))
      while i + 1 < strippedLines.length &&
        (sb.toString.trim.endsWith(".") || strippedLines(i + 1).trim.startsWith("."))
      do
        sb.append(' ').append(strippedLines(i + 1))
        i += 1
      result += ((squeezeDots(sb.toString), start))
      i += 1
    result.toList

  def validate(code: String, strictImportAliases: Boolean = true): List[Violation] =
    // Backticks may quote ordinary identifiers (`Runtime.`rootIO``,
    // `java.`io``) without changing what they resolve to. Blank them so the
    // dotted-token normalization and forbidden patterns see the real name while
    // character offsets/newlines remain stable for diagnostics.
    val stripped = stripLiteralsAndComments(code).replace('`', ' ')
    val originalLines = code.linesIterator.toArray
    val stringStripped = stripStringLiteralsOnly(code).replace('`', ' ').linesIterator.zipWithIndex.toList
    val logical = logicalLines(stripped.linesIterator.toArray)
    /** The violation of `pattern` on line `idx`, quoting the original source. */
    def violation(pattern: Forbidden, idx: Int, fallback: String): Violation =
      Violation(pattern.id, pattern.description, idx + 1, originalLines.lift(idx).getOrElse(fallback).trim)
    val perLine =
      for
        pattern <- forbidden
        lines = if pattern.keepComments then stringStripped else logical
        (line, idx) <- lines
        if pattern.regex.findFirstIn(line).isDefined
      yield violation(pattern, idx, line)
    def at(id: String, description: String, pos: Int): Violation =
      val idx = stripped.substring(0, pos).count(_ == '\n')
      Violation(id, description, idx + 1, originalLines.lift(idx).getOrElse("").trim)
    val catches = CatchArms.scan(stripped)
    // A multi-line ascription is invisible to the per-line `catch-fatal` regex
    // (`case _:\n Throwable =>`), so the arm scanner reports that case.
    val fatalArms = catches.typedArms.collect:
      case (offset, typed) if typed.contains('\n') && FatalTypeRe.findFirstIn(typed).isDefined =>
        at("catch-fatal", CatchFatalDescription, offset)
    val catchIssues = catches.catchAlls.map(at("catch-all", CatchAllDescription, _)) ++
      catches.extractors.map(at("catch-extractor", CatchExtractorDescription, _)) ++ fatalArms ++
      catches.handlers.map(at("catch-handler", CatchHandlerDescription, _))
    val aliases = importAliasOffsets(stripped)
    val importAliases =
      aliases.fatal.map(at("import-fatal-alias", FatalImportAliasDescription, _)) ++
        (if strictImportAliases then aliases.all.map(at("import-alias", ImportAliasDescription, _)) else Nil)
    val abstractTypes = CatchArms.abstractTypeNames(stripped)
    val abstractTypeCatches =
      if abstractTypes.isEmpty then Nil
      else
        catches.typedArms.collect:
          case (offset, typed) if CatchArms.topLevelTypeNames(typed).exists(abstractTypes.contains) =>
            at("catch-type-param", TypeParamCatchDescription, offset)
    perLine ++ catchIssues ++ importAliases ++ abstractTypeCatches

  def formatErrors(violations: List[Violation]): String =
    val header = s"Code validation failed (${violations.size} violation${if violations.size > 1 then "s" else ""}):"
    val details = violations.map(v => s"  [${v.ruleId}] Line ${v.lineNumber}: ${v.description}\n    > ${v.snippet}")
    (header :: details).mkString("\n")
