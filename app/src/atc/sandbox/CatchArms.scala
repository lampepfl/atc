package atc.sandbox

import CodeValidator.{isIdentChar, isIdentStart, keywordAt}

import scala.annotation.tailrec
import scala.collection.mutable

/** The analysis behind [[CodeValidator]]'s catch rules: the arms of every `catch`, and the
  * names that stand for an unknown type at run time. It reads code whose string literals
  * and comments are already blanked. */
private[sandbox] object CatchArms:
  private inline def isSpace(c: Char): Boolean = c == ' ' || c == '\t' || c == '\n' || c == '\r'

  /** What [[scan]] found: the character offsets of the offending `case` keywords
    * (`catchAlls` and `extractors`), every typed arm as its offset and the text of its
    * ascription (`typedArms`), and the offsets of `catch` handlers that are not `case`
    * arms at all (`handlers`: a PartialFunction value hides its arms from every rule). */
  final case class Scan(
    catchAlls: List[Int],
    extractors: List[Int],
    typedArms: List[(Int, String)],
    handlers: List[Int]
  )

  /** The arms of every `catch` in `code`, in one linear pass. Only `case`s belonging to a
    * `catch` are considered, so `match` arms and `.recover { case _ => }` are untouched.
    * An arm is a bare catch-all when its head is `_` or a lower-case binder followed by
    * `=>` or `if`, which also catches the ThreadDeath stop signal and fatal errors.
    * `case _: T` is typed instead. Any other arm is an extractor or identifier pattern
    * (`case NonFatal(e)`, `case a op b`, `case Stop`), whose `unapply` or `equals` is code
    * that can match any throwable. Braceless regions end at `finally`, a depth-0 `}`, or a
    * line indented no deeper than the `catch` that does not start with `case`. A braceless
    * nested `match` with a `case _` inside such an arm is an accepted false positive;
    * braced nested matches are never flagged. */
  def scan(code: String): Scan =
    if !code.contains("catch") then return Scan(Nil, Nil, Nil, Nil) // no catch, no scan
    val len = code.length
    val hits = mutable.ListBuffer[Int]()
    val extractors = mutable.ListBuffer[Int]()
    val typedArms = mutable.ListBuffer[(Int, String)]()
    val handlers = mutable.ListBuffer[Int]()
    def skipWs(from: Int): Int =
      var k = from
      while k < len && isSpace(code.charAt(k)) do k += 1
      k
    def identEnd(from: Int): Int =
      var k = from
      while k < len && isIdentChar(code.charAt(k)) do k += 1
      k
    /** Whether the arm's pattern ends at `k`, at its `=>` or an `if` guard. */
    def armEnds(k: Int): Boolean =
      (code.charAt(k) == '=' && k + 1 < len && code.charAt(k + 1) == '>') || keywordAt(code, k, "if")
    /** The ascription starting after the `:` at `colon`, up to the arm's `=>` or guard.
      * An unmatched `)`/`]` ends it too: the arm's typed part may sit inside an
      * enclosing pattern (`case Wrapped(e: Exception) =>`, `case e @ (_: Exception) =>`),
      * and without that stop the scan would run past the arm into unrelated code. */
    def ascription(colon: Int): String =
      var k = colon + 1
      var depth = 0
      var done = false
      while k < len && !done do
        val c = code.charAt(k)
        if depth == 0 && armEnds(k) then done = true
        else if depth == 0 && (c == ')' || c == ']') then done = true
        else
          if c == '(' || c == '[' then depth += 1
          else if c == ')' || c == ']' then depth -= 1
          k += 1
      code.substring(colon + 1, k).nn
    /** One arm at `caseOffset`, its pattern starting at `i0`: either a bare catch-all,
      * or a typed arm, whose ascription is recorded for the rules that need to read it. */
    def checkArm(caseOffset: Int, i0: Int): Unit =
      if isBareCatchAll(i0) then hits += caseOffset
      else if !isTypedPattern(i0) then extractors += caseOffset
      else
        val colon = code.indexOf(':', i0)
        if colon >= 0 && colon < code.indexOf("=>", i0).max(colon + 1) then
          typedArms += ((caseOffset, ascription(colon)))
    /** Whether the arm whose pattern starts at `i0` begins with a bare catch-all:
      * `_`, a lower-case binder, an `@`-binder over one, or any of those in
      * parentheses (`case (e) =>`, `case e @ _ =>`), with no type ascription
      * (a `:` is left to `catch-fatal`), extractor (upper-case name) or literal. */
    def isBareCatchAll(i0: Int): Boolean =
      @tailrec def from(k: Int, sawBinder: Boolean): Boolean =
        if k >= len then false
        else
          val c = code.charAt(k)
          if armEnds(k) then sawBinder
          else if c == ':' then false // a type ascription: left to `catch-fatal`
          else if c == '(' || c == ')' || c == '@' || c == '|' || isSpace(c) then from(k + 1, sawBinder)
          else if c == '_' then from(k + 1, true)
          else if isIdentStart(c) && c.isLower then from(identEnd(k), true)
          else false // upper-case extractor, a `.`-path, or a literal: not a bare catch-all
      from(skipWs(i0), false)
    /** Whether the arm whose pattern starts at `i0` holds only binders (`_`, a lower-case
      * name, `@`), alternatives, parentheses and type ascriptions. A `|` or `,` at the
      * level of an ascription ends it, since `case _: A | B() =>` is two alternatives. An
      * identifier followed by `(` is an extractor even inside an ascription. */
    def isTypedPattern(i0: Int): Boolean =
      val operatorChars = "!#%&*+-/:<=>?@\\^|~"
      var k = i0
      var depth = 0
      var typeDepth = -1 // the depth of the ascription being read, or -1 outside one
      var typed = true
      var done = false
      while k < len && typed && !done do
        val c = code.charAt(k)
        if depth == 0 && armEnds(k) then done = true
        else if c == '(' || c == '[' then
          depth += 1
          k += 1
        else if c == ')' || c == ']' then
          depth -= 1
          if typeDepth > depth then typeDepth = -1
          k += 1
        else if isIdentStart(c) then
          val end = identEnd(k)
          val next = skipWs(end)
          val following = if next < len then code.charAt(next) else ' '
          if following == '(' then typed = false
          else if typeDepth < 0 && (!(c.isLower || c == '_') || following == '.') then typed = false
          k = end
        else if operatorChars.contains(c) then
          var end = k
          while end < len && operatorChars.contains(code.charAt(end)) do end += 1
          code.substring(k, end) match
            case ":" => typeDepth = depth
            case "|" => if typeDepth == depth then typeDepth = -1
            case "@" => () // a binder, or an annotation inside an ascription
            case _ => if typeDepth < 0 then typed = false // an infix extractor
          k = end
        else if c == ',' then
          if typeDepth == depth then typeDepth = -1
          k += 1
        else if isSpace(c) || typeDepth >= 0 then k += 1
        else typed = false // a literal or a `.`-path
      typed
    /** Braced form: arms are the depth-1 `case`s inside `catch { ... }`. */
    def scanBraced(open: Int): Unit =
      var k = open + 1
      var depth = 1
      while k < len && depth > 0 do
        val c = code.charAt(k)
        if c == '{' then
          depth += 1
          k += 1
        else if c == '}' then
          depth -= 1
          k += 1
        else if depth == 1 && isIdentStart(c) then
          val end = identEnd(k)
          if keywordAt(code, k, "case") then checkArm(k, end)
          k = end
        else k += 1
    /** Braceless form: arms at depth 0 until the region ends (see above). */
    def scanBraceless(from: Int, catchIndent: Int): Unit =
      var k = from
      var depth = 0
      var stop = false
      while k < len && !stop do
        val c = code.charAt(k)
        if c == '{' then
          depth += 1
          k += 1
        else if c == '}' then
          if depth == 0 then stop = true else depth -= 1
          k += 1
        else if c == '\n' then
          // The next line: a `case` is a further arm; a blank line is skipped.
          var tok = k + 1
          var ind = 0
          var scanning = true
          while tok < len && scanning do
            code.charAt(tok) match
              case ' ' => ind += 1; tok += 1
              case '\t' => ind += 8; tok += 1
              case '\n' => ind = 0; tok += 1
              case _ => scanning = false
          if tok >= len then stop = true
          else if keywordAt(code, tok, "case") then () // a further arm; keep scanning
          else if keywordAt(code, tok, "finally") then stop = true
          else if ind <= catchIndent then stop = true
          k += 1
        else if depth == 0 && isIdentStart(c) then
          val end = identEnd(k)
          if keywordAt(code, k, "case") then checkArm(k, end)
          k = end
        else k += 1
    var i = 0
    while i < len do
      if isIdentStart(code.charAt(i)) then
        val end = identEnd(i)
        if keywordAt(code, i, "catch") then
          val j = skipWs(end)
          if j < len && code.charAt(j) == '{' then
            // `catch { h }` is a handler value as much as `catch h`: the arms must be right there.
            if keywordAt(code, skipWs(j + 1), "case") then scanBraced(j) else handlers += i
          else if !keywordAt(code, j, "case") then handlers += i
          else
            // the indent of the catch keyword's own line
            var s = i
            while s > 0 && code.charAt(s - 1) != '\n' do s -= 1
            var ind = 0
            while s < len && (code.charAt(s) == ' ' || code.charAt(s) == '\t') do
              ind += (if code.charAt(s) == '\t' then 8 else 1)
              s += 1
            scanBraceless(j, ind)
        i = end
      else i += 1
    Scan(hits.toList, extractors.toList, typedArms.toList, handlers.toList)

  // Catching a name that stands for an unknown type at run time erases to a catch of
  // its bound (Object for the default/`Any`/`AnyRef` bound), so `case _: T` inside
  // `def f[T]`, and `case _: X` under an abstract `type X`, swallow fatal throwables
  // exactly like `case _: Throwable`. Both are demonstrated to catch a real
  // StackOverflowError in the sandbox. A regex cannot tell such a name from a concrete
  // class, so correlate: collect the names declared as type parameters or as abstract
  // type members, then reject a catch arm whose ascription names one of them.
  private val typeParamDecl = raw"\b(?:def|class|trait|enum|given|extension|type)\b[^\[\n=]*?\[([^\]\n]*)\]".r
  /** A type-member declaration; group 2 is `=` exactly when it is a concrete alias. */
  private val typeMemberDecl = raw"\btype\s+([A-Za-z_]\w*)\s*(?:\[[^\]\n]*\])?\s*(=?)".r

  /** Split a type-parameter list on its top-level commas (a comma inside a nested
    * `[...]`/`(...)`, as in `[T <: Map[K, V]]`, does not separate parameters). */
  private def splitTopLevel(inner: String): List[String] =
    val segs = mutable.ListBuffer[String]()
    var depth = 0
    var start = 0
    var i = 0
    while i < inner.length do
      inner.charAt(i) match
        case '[' | '(' => depth += 1
        case ']' | ')' => depth -= 1
        case ',' if depth == 0 => segs += inner.substring(start, i); start = i + 1
        case _ => ()
      i += 1
    segs += inner.substring(start)
    segs.toList

  /** The names declared anywhere in `code` as a type parameter (variance and bounds
    * stripped: `+A`, `T <: X` and `F[_]` all yield their leading identifier) or as an
    * abstract type member (`type X`, `type X <: Foo`). A concrete alias (`type X = Foo`)
    * is not one of them: `case _: X` then tests Foo, and `catch-fatal-alias` covers an
    * alias of a fatal type. */
  def abstractTypeNames(code: String): Set[String] =
    val parameters = typeParamDecl
      .findAllMatchIn(code)
      .flatMap(m => splitTopLevel(m.group(1).nn))
      .map(seg => seg.trim.stripPrefix("+").stripPrefix("-").trim.takeWhile(isIdentChar))
      .filter(_.nonEmpty)
    val members = typeMemberDecl.findAllMatchIn(code).filter(_.group(2) != "=").map(_.group(1).nn)
    (parameters ++ members).toSet

  /** The undotted type names at the top level of a type ascription, which is what a
    * `case _: T` arm tests at run time. Names inside type arguments are skipped:
    * `case _: List[T]` erases to a test of List, not of T. */
  def topLevelTypeNames(ascription: String): Set[String] =
    val names = mutable.Set[String]()
    var depth = 0
    var i = 0
    while i < ascription.length do
      val c = ascription.charAt(i)
      if c == '[' then
        depth += 1
        i += 1
      else if c == ']' then
        depth -= 1
        i += 1
      else if isIdentStart(c) then
        val start = i
        while i < ascription.length && isIdentChar(ascription.charAt(i)) do i += 1
        val qualified = (start > 0 && ascription.charAt(start - 1) == '.') ||
          (i < ascription.length && ascription.charAt(i) == '.')
        if depth == 0 && !qualified then names += ascription.substring(start, i).nn
      else i += 1
    names.toSet
