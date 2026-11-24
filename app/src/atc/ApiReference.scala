package atc

/** The agent API as the model reads it, from the `Interface.scala` the build bundles: the full
  * source, a compact form that cuts each doc comment to its first sentence, and the whole
  * documentation of one name, which `help` prints for the compact form's readers. */
object ApiReference:
  /** The bundled source, or a note when it is missing (a classpath the build did not produce). */
  private[atc] lazy val bundled: String =
    Resources.text("/atc/Interface.scala.txt").getOrElse("(API reference unavailable)")

  /** The source without the [[KeepWhole]] markers. */
  lazy val full: String = bundled.split("\n", -1).filterNot(isMarker).mkString("\n")

  lazy val compact: String = compact(bundled)

  /** A line of its own in a doc comment that the compact form keeps whole; neither form shows it. */
  val KeepWhole = "@keepWhole"
  private def isMarker(line: String): Boolean = line.trim.stripPrefix("*").trim == KeepWhole

  /** The keyword and name of a declaration, at the start of a trimmed line. */
  private val Declaration =
    """(?:(?:abstract|case|sealed|final|override|private(?:\[\w+\])?|update)\s+)*(def|val|class|trait|object|enum|case)\s+([A-Za-z_][A-Za-z0-9_]*).*""".r
  private val TypeKeywords = Set("class", "trait", "object", "enum")

  private def indentOf(line: String): Int = line.takeWhile(_ == ' ').length

  /** Where the ` = ` that starts a method body is, outside brackets; none for an abstract method. */
  private def bodyStart(line: String): Option[Int] =
    var depth = 0
    line.indices
      .find: i =>
        line.charAt(i) match
          case '(' | '[' | '{' => depth += 1; false
          case ')' | ']' | '}' => depth -= 1; false
          case '=' => depth == 0 && line.startsWith(" =", i - 1) && (i + 1 == line.length || line.charAt(i + 1) == ' ')
          case _ => false
      .map(_ - 1)

  /** The first sentence of `text`: up to a `.`, `!` or `?` that is outside backticks, ends a
    * word other than an abbreviation, and comes before a space or the end. */
  private[atc] def firstSentence(text: String): String =
    var inCode = false
    var end = -1
    var i = 0
    while end < 0 && i < text.length do
      val c = text.charAt(i)
      if c == '`' then inCode = !inCode
      else if !inCode && ".!?".contains(c) && (i + 1 == text.length || text.charAt(i + 1) == ' ') then
        val word = text.take(i).reverse.takeWhile(ch => !ch.isWhitespace && ch != '(').reverse
        if !Set("e.g", "i.e", "etc", "vs").contains(word) then end = i + 1
      i += 1
    if end < 0 then text else text.take(end)

  /** A doc comment as one line holding its first sentence, `…` marking a cut. */
  private def cutDoc(comment: Vector[String]): String =
    val lines = comment.map(_.trim.stripPrefix("/**").stripSuffix("*/").trim.stripPrefix("*").trim)
    val prose = lines.takeWhile(!_.startsWith("{{{"))
    val text = prose.filter(_.nonEmpty).mkString(" ")
    val sentence = firstSentence(text)
    val cut = prose.size < lines.size || sentence.length < text.length
    " " * indentOf(comment.head) + "/** " + sentence + (if cut then " …" else "") + " */"

  /** `lines` without method bodies, private and overriding members, annotations, imports,
    * empty companion objects and [[KeepWhole]] markers. With `cutDocs`, each doc comment
    * without a marker keeps its first sentence, and only the header comment and the section
    * dividers of the `//` comments stay. */
  private def strip(lines: Vector[String], cutDocs: Boolean): Vector[String] =
    val out = Vector.newBuilder[String]
    var header = false
    var i = 0
    while i < lines.length do
      val line = lines(i)
      val trimmed = line.trim
      val indent = indentOf(line)
      def skipBody(): Unit =
        while i + 1 < lines.length && lines(i + 1).trim.nonEmpty && indentOf(lines(i + 1)) > indent do i += 1
      if trimmed.startsWith("package ") || trimmed.startsWith("import ") || trimmed.startsWith("@") ||
        trimmed.matches("object \\w+")
      then ()
      else if trimmed.startsWith("//") then
        if trimmed.contains("═══") then header = true
        else if trimmed.contains("───") then header = false
        if !cutDocs || header || trimmed.contains("──") then out += line
      else if trimmed.startsWith("/**") then
        val start = i
        while i + 1 < lines.length && !lines(i).contains("*/") do i += 1
        val doc = lines.slice(start, i + 1)
        val kept = doc.filterNot(isMarker)
        if cutDocs && !doc.exists(isMarker) then out += cutDoc(kept) else out ++= kept
      else if trimmed.startsWith("private ") || trimmed.startsWith("override ") then skipBody()
      else
        val isDef = trimmed.startsWith("def ") || trimmed.startsWith("update def ")
        (if isDef then bodyStart(line) else None) match
          case Some(body) =>
            out += line.take(body)
            skipBody()
          case None => out += line.replace(" private[atc] ()", "")
      i += 1
    // At most one blank line in a row.
    val kept = out.result().foldLeft(Vector.empty[String]): (kept, line) =>
      if line.trim.isEmpty && kept.lastOption.forall(_.trim.isEmpty) then kept else kept :+ line
    // A type whose members were all removed (`ProcessResult` without its `toString`) loses its colon.
    kept.indices.toVector.map: i =>
      val line = kept(i)
      val next = kept.drop(i + 1).find(_.trim.nonEmpty)
      if line.endsWith(":") && Declaration.matches(line.trim) && next.forall(indentOf(_) <= indentOf(line))
      then line.dropRight(1)
      else line

  /** `source` with each doc comment cut to its first sentence, unless a [[KeepWhole]] marker
    * keeps it whole, and without code examples, method bodies, private and overriding members,
    * annotations, imports and empty companion objects. The header on reading capture types and
    * the section dividers stay. */
  def compact(source: String): String =
    strip(source.split("\n", -1).toVector, cutDocs = true).mkString("\n").trim + "\n"

  /** A declaration in the source: its line, name and the type it belongs to ("" at the top). */
  private final case class Declared(line: Int, name: String, owner: String, isType: Boolean)

  private def declarations(lines: Vector[String]): Vector[Declared] =
    var owners = List.empty[(Int, String)] // enclosing types, innermost first, with their indent
    var inDoc = false
    lines.indices.toVector.flatMap: i =>
      val trimmed = lines(i).trim
      val wasDoc = inDoc
      if trimmed.startsWith("/**") then inDoc = !trimmed.contains("*/")
      else if inDoc && trimmed.contains("*/") then inDoc = false
      if wasDoc || trimmed.startsWith("/**") || trimmed.startsWith("//") then None
      else
        trimmed match
          case Declaration(kw, n) =>
            val (keyword, name) = (kw.nn, n.nn)
            val indent = indentOf(lines(i))
            owners = owners.dropWhile(_._1 >= indent)
            val owner = owners.headOption.fold("")(_._2)
            val isType = TypeKeywords.contains(keyword)
            if isType && trimmed.endsWith(":") then owners = (indent, name) :: owners
            Some(Declared(i, name, owner, isType))
          case _ => None

  /** The comments right above line `i`: its doc comment and `//` lines, past annotations. */
  private def commentsAbove(lines: Vector[String], i: Int): Vector[String] =
    var start = i
    var inDoc = false
    var going = true
    while going && start > 0 do
      val above = lines(start - 1).trim
      if inDoc then
        start -= 1
        if above.startsWith("/**") then inDoc = false
      else if above.endsWith("*/") then
        start -= 1
        inDoc = !above.startsWith("/**")
      else if above.startsWith("@") || above.startsWith("//") && !above.contains("──") then start -= 1
      else going = false
    lines.slice(start, i).filterNot(_.trim.startsWith("@"))

  /** Line `i`'s declaration with what belongs to it: a method's continued signature, a type's members. */
  private def declaredBlock(lines: Vector[String], i: Int): Vector[String] =
    val indent = indentOf(lines(i))
    val rest = lines.drop(i + 1).takeWhile(l => l.trim.isEmpty || indentOf(l) > indent)
    (lines(i) +: rest).reverse.dropWhile(_.trim.isEmpty).reverse

  /** The whole documentation of `query` in `source`: each declaration of that name with its
    * doc comment, a type with its members; `Type.member` keeps one type's. For an unknown
    * name, the names that contain it or that it contains. */
  def help(source: String, query: String): String =
    val lines = source.split("\n", -1).toVector
    val all = declarations(lines)
    val (owner, name) = query.trim.lastIndexOf('.') match
      case -1 => (None, query.trim)
      case dot => (Some(query.trim.take(dot)), query.trim.drop(dot + 1))
    val found = all.filter(d => d.name == name && owner.forall(_ == d.owner))
    if found.isEmpty then
      val q = name.toLowerCase
      val similar = all.map(_.name).distinct
        .filter(n => n.toLowerCase.contains(q) || n.length >= 3 && q.contains(n.toLowerCase))
        .sorted
      if similar.isEmpty then s"No API name `$query`."
      else s"No API name `$query`; similar names: ${similar.take(12).mkString(", ")}."
    else
      val labelled = found.map(_.owner).distinct.size > 1
      // Each declaration with its comments and lines; overloads that follow one another stay together.
      val blocks = found.map: d =>
        val above = commentsAbove(lines, d.line)
        val declared = declaredBlock(lines, d.line)
        (d, d.line - above.size, d.line + declared.size, strip(above ++ declared, cutDocs = false))
      val groups = blocks.filter(_._4.exists(_.trim.nonEmpty)).foldLeft(Vector.empty[(Declared, Int, Vector[String])]):
        case (groups :+ ((first, end, text)), (d, start, next, block)) if start == end && d.owner == first.owner =>
          groups :+ (first, next, text ++ block)
        case (groups, (d, _, end, block)) => groups :+ (d, end, block)
      groups.map: (d, _, block) =>
        val margin = block.filter(_.trim.nonEmpty).map(indentOf).minOption.getOrElse(0)
        val owner = if d.owner.isEmpty || d.owner == "Interface" then "" else d.owner + "."
        (Option.when(labelled)(s"// $owner${d.name}").toVector ++ block.map(_.drop(margin))).mkString("\n")
      .mkString("\n\n")
