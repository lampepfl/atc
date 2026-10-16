package atc.ui

import org.jline.utils.AttributedString

/** Width-aware wrapping for complete terminal lines. SGR styles, the only sequences in
  * text that reaches the screen, do not consume columns. */
private[atc] object TextLayout:
  /** `text` in rows of at most `columns` cells. Every row is written from screen column
    * `origin`, after a gutter or indent, and the terminal's tab stops count from column 0. */
  def wrap(text: String, columns: Int, origin: Int = 0): List[String] =
    val room = columns.max(2)
    text.split("\n", -1).toList.flatMap: line =>
      if Screen.displayWidth(line, origin) <= room then List(line)
      else
        val lines = List.newBuilder[String]
        val styled = AttributedString.fromAnsi(line).nn
        val plain = styled.toString
        var start = 0
        while start < plain.length do
          var end = start
          var used = 0
          var space = -1
          var full = false
          while end < plain.length && !full do
            val cp = plain.codePointAt(end)
            val size = Screen.cellWidth(cp, origin + used)
            if used + size > room then full = true
            else
              if Character.isWhitespace(cp) then space = end
              used += size
              end += Character.charCount(cp)
          val cut = if full && space > start && space - start >= (end - start) / 2 then space else end
          lines += styled.subSequence(start, cut).nn.toAnsi().nn
          start = cut
          while start < plain.length && plain.charAt(start).isWhitespace do start += 1
        lines.result()

  /** Keep labels beside their values when there is room, otherwise place values below them. */
  def fields(rows: List[(String, String)], columns: Int): List[String] =
    val labelWidth = rows.map(row => Screen.displayWidth(row._1)).maxOption.getOrElse(0)
    val valueWidth = columns - labelWidth - 5
    rows.flatMap: (label, value) =>
      if valueWidth < 28 then
        wrap(label, columns - 3).map("  " + _) ++ wrap(value, columns - 5).map("    " + _)
      else
        val prefix = "  " + label + " " * (labelWidth - Screen.displayWidth(label) + 2)
        val continuation = " " * (labelWidth + 4)
        wrap(value, valueWidth).zipWithIndex.map((line, index) => (if index == 0 then prefix else continuation) + line)
