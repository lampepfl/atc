package atc.ui

import atc.host.FileChange
import atc.sandbox.{ExecutionResult, ReplSession}

import scala.collection.mutable

import Ansi.{Bold, Cyan, Dim, Green, Magenta, Red, Yellow}

/** The block of one tool call: its code, the program output shown live, then what the
  * REPL returned, the files it changed and the verdict. Results are kept for `/output`
  * in [[history]]. Called with the screen's lock held.
  *
  * In the compact view (the default on a terminal) the running block is one live
  * region: the start of the code and the tail of the output, kept shorter than the
  * screen so it can always be redrawn. When the call ends the region is replaced by a
  * summary: the code's first line, the files changed and the verdict, with the rest in
  * `/output`. The expanded view (Ctrl-O) and a plain terminal write everything out. */
private[ui] final class ToolBlock(screen: Screen, expanded: () => Boolean):
  import screen.{GutterWidth, Indent, ensureNewline, g, gutter, plain, styled, width, write}

  /** Recent results for `/output`. */
  val history: ToolHistory = ToolHistory()
  /** A tool block is open; `outputStarted` once its output section has begun. */
  private var open = false
  private var outputStarted = false
  /** Cells on the output's current row, where the expanded view breaks long lines. */
  private var outputColumn = 0
  /** Agent-visible text printed during the current tool call, as it appears in
    * the REPL's captured output; `end` subtracts it from the result panel. */
  private val printed = StringBuilder()
  private val liveCaptured = StringBuilder()
  private var liveTruncated = false
  private val fileChanges = mutable.ListBuffer[FileChange]()
  private var currentCode = ""
  private var title = ""
  /** The code as the block shows it, one (highlighted) line each. */
  private var codeLines = List.empty[String]
  /** The block's display-ready output: every line counted, the tail kept. */
  private val output = TailBuffer(TailBuffer.MaxChars)

  /** The compact view draws this block in `region`. `headed` while the region still
    * holds the title and code; a pop-up or Ctrl-O leaves them above it, and the region
    * then shows only the output after `shownFrom` lines. */
  private var compact = false
  private var region: Option[screen.LiveRegion] = None
  private var headed = false
  private var shownFrom = 0L

  def isOpen: Boolean = open

  /** Open a code block titled `title`. */
  def start(code: String, title: String): Unit =
    this.title = title
    currentCode = code
    // The code is model-written: sanitize before highlighting/printing.
    val clean = Ansi.sanitize(code)
    codeLines = if screen.colors > 0 then Highlight.scala(clean) else clean.linesIterator.toList
    wrappedWidth = -1
    open = true
    outputStarted = false
    printed.clear()
    liveCaptured.clear()
    liveTruncated = false
    fileChanges.clear()
    output.clear()
    // While the block is open, `compact == !plain && !expanded()`: Ctrl-O flips `expanded` and
    // calls `reattach` in one frame. So `end` writes the result out only on a plain terminal
    // or in the expanded view.
    compact = !plain && !expanded()
    if compact then
      headed = true
      shownFrom = 0
      redraw()
    else writeHead()

  private def titleRow(title: String): String = styled(g.bullet, Magenta) + " " + styled(title, Magenta, Bold)

  /** The code's rows at `wrappedWidth`, kept: the compact view redraws on every output chunk. */
  private var wrapped = List.empty[String]
  private var wrappedWidth = -1
  private def codeRows: List[String] =
    if wrappedWidth != width then
      wrapped =
        codeLines.flatMap(line => TextLayout.wrap(line, width - GutterWidth - 1, GutterWidth)).map(gutter(Magenta) + _)
      wrappedWidth = width
    wrapped

  private def writeHead(): Unit =
    write(titleRow(title) + "\n")
    codeRows.foreach(row => write(row + "\n"))

  def fileChanged(change: FileChange): Unit =
    if open && fileChanges.size < 50 then fileChanges += change

  /** The agent's `println`. Classified content, where the two texts differ, is marked so
    * the user knows the model cannot see it. `printed` keeps the raw text, which `end`
    * matches verbatim against the REPL capture; only the display is sanitized. */
  def print(agentText: String, userText: String): Unit =
    // Text beyond the REPL capture limit cannot be subtracted from its result. The
    // limit is in bytes and this length in chars, so the budget is approximate.
    val room = ReplSession.MaxOutputBytes - printed.length
    if room > 0 then printed.append(agentText.take(room))
    if agentText == userText then emit(Ansi.sanitize(userText))
    else
      // Every line is marked: the gutter at each line start resets the style.
      val marked = Ansi.sanitize(userText).split("\n", -1).map: line =>
        if line.isEmpty then line else styled("[classified] ", Yellow, Bold) + styled(line, Yellow)
      emit(marked.mkString("\n"))

  /** What a command the agent runs writes, kept for `/output` but not part of the tool result. */
  def commandOutput(text: String): Unit =
    val room = TailBuffer.MaxChars - liveCaptured.length
    if room > 0 then liveCaptured.append(text.take(room))
    if text.length > room then liveTruncated = true
    emit(Ansi.sanitize(text))

  /** Display-ready text in the output section, which the first output opens. While a
    * pop-up reads (`hold`), the text waits in `held` for `release`. */
  def emit(text: String): Unit =
    if holding then held.append(text)
    else
      screen.stopSpinner()
      output.append(text)
      if compact then
        outputStarted = true
        redraw()
      else writeOutput(text)

  /** Write `text` out in the output section, which the first output opens. Long lines break
    * at the row's end, so they stay inside the gutter. */
  private def writeOutput(text: String): Unit =
    if open && !outputStarted then
      ensureNewline()
      write(section("output", Dim))
      outputStarted = true
      outputColumn = 0
    val (rows, column) =
      if plain then (text, 0) else Screen.breakLines(text, outputColumn, width - GutterWidth - 1, GutterWidth)
    outputColumn = column
    screen.writeGuttered(rows, gutter(Dim))

  /** Output of `parallel` tasks that arrives while a pop-up is drawn, which writing it
    * would break; the last [[TailBuffer.MaxChars]] characters are kept. */
  private val held = TailBuffer(TailBuffer.MaxChars)
  private var holding = false

  def hold(): Unit = holding = true

  /** The pop-up closed: write what arrived meanwhile. */
  def release(): Unit =
    holding = false
    val text = held.text
    held.clear()
    if text.nonEmpty then emit(text)

  private def section(label: String, code: Int): String =
    Indent + styled(s"${g.tee} $label", code) + "\n"

  // ── the compact view ──────────────────────────────────────────────

  private def liveRegion(): screen.LiveRegion = region.getOrElse { val r = screen.LiveRegion(); region = Some(r); r }

  private def redraw(force: Boolean = false): Unit = liveRegion().redraw(liveRows(), force)

  /** The running block: the title and the start of the code, then the tail of the
    * output. Code gives way first when the screen is short: the rows must fit
    * above the footer, or the region could not be redrawn. The output's tail is
    * read and cut to the width once; a line can be as long as the whole buffer. */
  private def liveRows(): List[String] =
    val room = (screen.height - 3).max(4)
    val code = codeRows
    val lines = output.lineCount - shownFrom
    val lastLines =
      output.tail(ToolBlock.OutputTailRows.toLong.min(lines).toInt).map(l => gutter(Dim) + screen.fit(l, GutterWidth))
    def moreLines(hidden: Long, bar: Int): String =
      gutter(bar) + styled(s"${g.ellipsis} ${Format.plural(hidden, "more line")}", Dim)
    def codePart(max: Int): List[String] =
      if code.size <= max then code else code.take(max - 1) :+ moreLines(code.size - max + 1, Magenta)
    def outputPart(max: Int): List[String] =
      val tail = lastLines.takeRight(max)
      val hidden = lines - tail.size
      Indent + styled(s"${g.tee} output", Dim) :: Option.when(hidden > 0)(moreLines(hidden, Dim)).toList ++ tail
    def rows(codeMax: Int, tailMax: Int): List[String] =
      (if headed then titleRow(title) :: codePart(codeMax) else Nil) ++
        (if outputStarted then outputPart(tailMax) else Nil)
    var codeMax = ToolBlock.CodePreviewRows
    var tailMax = ToolBlock.OutputTailRows
    while rows(codeMax, tailMax).size > room && codeMax > 1 do codeMax -= 1
    while rows(codeMax, tailMax).size > room && tailMax > 1 do tailMax -= 1
    rows(codeMax, tailMax)

  /** The finished block: the title with the code's first line, the files changed and
    * the verdict, each cut to one row by the region. */
  private def summaryRows(r: ExecutionResult, millis: Long, id: Int): List[String] =
    val first = Ansi.sanitize(currentCode).linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
    val more = if currentCode.trim.linesIterator.size > 1 then s" ${g.ellipsis}" else ""
    val head = Option.when(headed)(titleRow(title) + "  " + styled(first + more, Dim))
    val start = verdict(r.success, millis)
    val link = outputLink(id)
    val detail =
      if r.success then
        val lines = output.lineCount
        if lines > 0 then styled(s" ${g.dot} ${Format.plural(lines, "line")} of output", Dim) else ""
      else
        ToolBlock.firstProblem(r, printed.toString).map(Ansi.sanitize).fold(""): problem =>
          val used = Screen.displayWidth(start) + Screen.displayWidth(link) + 3
          styled(s" ${g.dot} ", Red) + styled(screen.fit(problem, used), Red)
    head.toList ++ changeRows(fileChanges.toList) :+ (start + detail + link)

  /** The start of the verdict row: `└ ok 12 ms` or `└ failed 12 ms`. */
  private def verdict(success: Boolean, millis: Long): String =
    val (word, colour) = if success then ("ok", Green) else ("failed", Red)
    Indent + styled(s"${g.end} $word $millis ms", colour)

  /** Where the whole result can be read again. */
  private def outputLink(id: Int): String = styled(s" ${g.dot} /output $id", Dim)

  /** A row per file change, with a change repeated in a row (four one-line edits of a
    * file) folded into one row that counts them. */
  private def changeRows(changes: List[FileChange]): List[String] =
    val folded =
      changes.map(change => s"${change.path}: ${change.summary}").foldRight(List.empty[(String, Int)]):
        case (line, (same, n) :: rest) if same == line => (same, n + 1) :: rest
        case (line, rest) => (line, 1) :: rest
    folded.map((line, n) => Indent + styled(Ansi.sanitize(if n == 1 then line else s"$line, $n times"), Cyan))

  // ── the end of a block ────────────────────────────────────────────

  /** Close the block. The compact view replaces it with its summary; otherwise what
    * the REPL produced besides the agent's own prints (those were shown live), meaning
    * diagnostics, echoed values and exceptions, follows, then the verdict. */
  def end(r: ExecutionResult, millis: Long): Unit =
    val liveText = liveCaptured.toString + (if liveTruncated then "\n[retained live output limit reached]" else "")
    val record = history.add(title, currentCode, r, millis, liveText, fileChanges.toList)
    screen.stopSpinner()
    if compact then
      val live = liveRegion()
      live.redraw(summaryRows(r, millis, record.id), force = true)
      live.freeze()
      region = None
    else writeResult(r, millis, record.id)
    open = false
    compact = false

  private def writeResult(r: ExecutionResult, millis: Long, id: Int): Unit =
    ensureNewline()
    val body =
      List(Option(r.output).filter(_.nonEmpty), r.error).flatten.mkString("\n")
    val lines = ToolBlock.withoutPrinted(body, printed.toString).linesIterator.toList
    if lines.nonEmpty then
      // REPL output holds the agent's raw prints and compiler diagnostics: sanitize
      // before display. The subtraction above runs on the raw text, before this.
      val cleaned = lines.map(Ansi.sanitize).flatMap(outputRows)
      if r.success then
        write(section("result", Dim))
        cleaned.foreach(l => write(gutter(Dim) + styled(l, Dim) + "\n"))
      else
        write(section("error", Red))
        cleaned.foreach(l => write(gutter(Red) + l + "\n"))
    changeRows(fileChanges.toList).foreach(row => write(row + "\n"))
    write(verdict(r.success, millis) + outputLink(id) + "\n")

  /** A line of output in rows inside the gutter. A compiler heading wider than the row
    * (`-- [E007] Type Mismatch Error: ----`) has its rule cut to the row instead of wrapped. */
  private def outputRows(line: String): List[String] =
    val room = width - GutterWidth - 1
    val heading = line.reverse.dropWhile(_ == '-').reverse
    if plain then List(line)
    else if line.startsWith("-- ") && line.endsWith("---") && Screen.displayWidth(line) > room
      && Screen.displayWidth(heading) + 3 <= room
    then List(heading + "-" * (room - Screen.displayWidth(heading)))
    else TextLayout.wrap(line, room, GutterWidth)

  /** Something else takes the screen (a pop-up, a notice): what the running block shows
    * stays as ordinary output, and what follows is drawn below it. */
  def endOutput(): Unit = region.foreach: live =>
    live.freeze()
    region = None
    headed = false
    outputStarted = false
    shownFrom = output.lineCount

  /** The turn is over: what the block shows stays, and no block is open. */
  def endTurn(): Unit =
    endOutput()
    open = false
    compact = false

  def resize(): Unit = if region.isDefined then redraw(force = true)

  /** Ctrl-O, first half: take the compact view's region down. */
  def detach(): Unit = region.foreach: live =>
    live.clear()
    region = None

  /** Ctrl-O, second half. The expanded view writes out the running block in full (its
    * output as far as it is kept) and continues directly; the compact view draws what
    * follows in a region under what is already written. */
  def reattach(): Unit = if open then
    if expanded() then
      if compact then
        compact = false
        if headed then writeHead()
        val lines = output.lineCount - shownFrom
        val started = outputStarted
        outputStarted = false // the region's output section is gone: a new one opens
        if started && lines > 0 then writeOutput(output.tail(lines.min(Int.MaxValue).toInt).mkString("", "\n", "\n"))
    else if !compact then
      compact = true
      headed = false
      outputStarted = false
      shownFrom = output.lineCount

  // ── /output ───────────────────────────────────────────────────────

  /** `/output`: a row per retained call, its number, title, verdict and the code's first line. */
  def writeIndex(): Unit =
    val entries = history.entries
    /** The cells padded to one width, on the left for numbers. */
    def column(cells: Vector[String], numbers: Boolean): Vector[String] =
      val w = cells.map(Screen.displayWidth(_)).maxOption.getOrElse(0)
      cells.map: c =>
        val pad = " " * (w - Screen.displayWidth(c))
        if numbers then pad + c else c + pad
    val ids = column(entries.map(_.id.toString), numbers = true)
    val titles = column(entries.map(e => Ansi.sanitize(e.title)), numbers = false)
    val times = column(entries.map(_.millis.toString), numbers = true)
    entries.indices.foreach: i =>
      val e = entries(i)
      val (word, colour) = if e.success then ("ok    ", Green) else ("failed", Red)
      val head = Indent + styled(ids(i), Bold) + "  " + styled(titles(i), Magenta) + "  " +
        styled(word, colour) + " " + styled(s"${times(i)} ms", Dim) + "  "
      val code = Ansi.sanitize(e.code).linesIterator.map(_.trim).filter(_.nonEmpty).toList
      val first = code.headOption.getOrElse("") + (if code.size > 1 then s" ${g.ellipsis}" else "")
      write(head + screen.fit(first, Screen.displayWidth(head)) + "\n")

  /** `/output <id> [line]`: retained call `e` written out as the expanded view writes a
    * block. Its code, output, command output and file previews count as one run of lines;
    * the page holds [[RetainedPage]] of them from line `from` on, each part under its
    * heading, and ends with the verdict or with where the next page starts. Nothing is
    * written when `from` is past the last line. Returns the number of lines. */
  def writeRetained(e: ToolHistory.Entry, from: Int): Int =
    def lines(text: String) = text.linesIterator.map(Ansi.sanitize).toList
    val code = Ansi.sanitize(e.code)
    val (result, resultBar) = if e.success then ("output", Dim) else ("error", Red)
    // Each part: its heading row, if any, the colour of its gutter and its lines.
    val parts: List[(Option[String], Int, List[String])] =
      (None, Magenta, if screen.colors > 0 then Highlight.scala(code) else code.linesIterator.toList) ::
        (Some(section(result, resultBar)), resultBar, lines(e.output)) ::
        Option.when(e.live.nonEmpty)((Some(section("command output", Dim)), Dim, lines(e.live))).toList :::
        e.changes.map: c =>
          (Some(section(Ansi.sanitize(s"${c.path}: ${c.summary}"), Cyan)), Dim, lines(c.preview).map(diffLine))
    val total = parts.map(_._3.size).sum
    if from <= total then
      val end = (from.toLong - 1 + ToolBlock.RetainedPage).min(total.toLong).toInt
      val page = if from > 1 || end < total then s" ${g.dot} lines $from to $end of $total" else ""
      write(titleRow(Ansi.sanitize(e.title)) + styled(s"  /output ${e.id}$page", Dim) + "\n")
      var first = 1 // the number of the part's first line
      parts.foreach: (heading, bar, partLines) =>
        val shown = partLines.slice(from - first, end - first + 1)
        if shown.nonEmpty then
          heading.foreach(h => write(h))
          shown.flatMap(outputRows).foreach(row => write(gutter(bar) + row + "\n"))
        first += partLines.size
      if end < total then
        val more = s"${g.end} ${Format.plural(total - end, "more line")} ${g.dot} /output ${e.id} ${end + 1}"
        write(Indent + styled(more, Dim) + "\n")
      else write(verdict(e.success, e.millis) + "\n")
    total

  /** A line of a file-change preview: additions green, removals red, the hunk and notes dim. */
  private def diffLine(line: String): String =
    if line.startsWith("@@") || line.startsWith("[") then styled(line, Dim)
    else if line.startsWith("+") then styled(line, Green)
    else if line.startsWith("-") then styled(line, Red)
    else line

private[ui] object ToolBlock:
  /** Rows of code and of output tail a running block shows in the compact view. */
  private val CodePreviewRows = 8
  private val OutputTailRows = 10
  /** Lines of a retained call that one `/output` page shows. */
  val RetainedPage = 200

  /** The first line that says why a call failed: its error, else the first line of
    * what the REPL wrote besides the agent's own prints. A compiler diagnostic's
    * heading (`-- [E007] Type Mismatch Error: ----`) loses its rule, and one without
    * an error code (`-- Error: ----`) gives way to its first message line. */
  private[ui] def firstProblem(r: ExecutionResult, printed: String): Option[String] =
    def first(text: String): Option[String] =
      val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
      lines.headOption.map: line =>
        if !line.startsWith("-- ") then line
        else
          val heading = line.stripPrefix("-- ").reverse.dropWhile(c => c == '-' || c == ':' || c == ' ').reverse
          // Source lines (`1 |code`) and carets come before the message, which follows a bare `|`.
          val message = lines.drop(1).filter(_.startsWith("|")).map(_.stripPrefix("|").trim)
            .find(m => m.nonEmpty && !m.forall(c => c == '^' || c == ' '))
          if heading.startsWith("[") then heading else message.getOrElse(heading)
    r.error.flatMap(first).orElse(first(withoutPrinted(r.output, printed)))
      .map(_.replaceFirst("^java\\.lang\\.(?=\\w+(?:Exception|Error)\\b)", ""))

  /** The REPL output without the agent's own prints. The host wrote those to
    * the same stream, so they occur verbatim and contiguously: remove the
    * first occurrence of `printed`. If it cannot be found (truncated capture),
    * the body is shown as is. */
  def withoutPrinted(body: String, printed: String): String =
    val p = printed.stripTrailing()
    if p.isEmpty then body
    else
      val i = body.indexOf(p)
      if i < 0 then body
      else
        val before = body.take(i).stripTrailing()
        val after = body.drop(i + p.length).stripLeading()
        if before.isEmpty then after
        else if after.isEmpty then before
        else before + "\n" + after
