package atc.ui

import org.jline.terminal.{Size, Terminal}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8

/** The screen: its measured size, rows fitted and broken to the width, live
  * regions and the row updates that keep the footer in place. */
class ScreenSuite extends munit.FunSuite:
  test("measuring again reports whether the terminal's size changed"):
    val terminal = TestTerminal(100, 30)
    val screen = Screen(terminal, plain = false, Glyphs.unicode)
    assertEquals((screen.width, screen.height), (100, 30))
    assert(!screen.resized(), "nothing changed")
    (terminal: Terminal).setSize(Size(60, 30))
    assertEquals(screen.width, 100, "measured only when asked")
    assert(screen.resized())
    assertEquals((screen.width, screen.height), (60, 30))

  test("fit counts tab stops from the column the text starts at, and a newline as a space"):
    val screen = TestTerminal.screen(80)
    // After a four-column gutter the tab reaches column 16, so the line would end at column 83.
    val tabbed = "xxxxx\t" + "y" * 67
    val cut = screen.fit(tabbed, 4)
    assert(cut.endsWith("…"), cut)
    assert(Screen.displayWidth("    " + cut) < 80, cut)
    assertEquals(screen.fit(tabbed, 0), tabbed, "from column 0 it fits")
    assertEquals(screen.fit("a\nb", 0), "a b")
    assertEquals(screen.fit("\u001b[2m" + "z" * 200 + "\u001b[0m", 0), "\u001b[2m" + "z" * 78 + "\u001b[0m…")

  test("output breaks count tab stops from the screen column after the gutter"):
    // From column 4 the tab after five cells reaches column 16: twelve cells, the whole room.
    assertEquals(Screen.breakLines("abcde\tfghij", 0, 12, origin = 4), ("abcde\t\nfghij", 5))

  test("a live region cuts every row to the width from column 0, gutter and all"):
    val out = ByteArrayOutputStream()
    val screen = TestTerminal.screen(24, out)
    val region = screen.LiveRegion()
    val rows =
      List(screen.gutter(Ansi.Dim) + "xxxxx\t" + "y" * 10, "• thinking… (Ctrl-O to expand)", "a label\nwith a newline")
    screen.frame(region.redraw(rows))
    val drawn = out.toString(UTF_8).split("\r\u001b\\[2K").toList.drop(1).map(_.stripSuffix("\n"))
    assertEquals(drawn.size, 3)
    assert(drawn.forall(row => !row.contains('\n') && Screen.displayWidth(row) < 24), drawn)
    assertEquals(drawn(2), "a label with a newline")

  test("live row updates preserve the footer while growing, shrinking and clearing the preview"):
    val cases = List(
      List("header", "old") -> List("header", "new"),
      List("first") -> List("first", "second"),
      List("first", "second", "third") -> List("first"),
      List("first", "second") -> Nil,
      Nil -> List("first"),
      List("same") -> List("same"),
    )
    val control = "\u001b\\[([0-9]+)([ABK])".r
    for
      (before, after) <- cases
      force <- List(false, true)
    do
      val rows = Array.fill(12)("")
      before.zipWithIndex.foreach((line, index) => rows(index) = line)
      rows(11) = "footer must remain"
      var row = before.size
      var column = 0
      var remaining = Screen.replaceRows(before, after, force)
      while remaining.nonEmpty do
        control.findPrefixMatchOf(remaining) match
          case Some(sequence) =>
            val count = sequence.group(1).nn.toInt
            sequence.group(2) match
              case "A" => row -= count
              case "B" => row += count
              case "K" => assertEquals(count, 2); rows(row) = ""
              case _ => fail("Unexpected cursor operation")
            remaining = remaining.drop(sequence.end)
          case None =>
            remaining.head match
              case '\r' => column = 0
              case '\n' => row += 1; column = 0
              case '\u001b' => fail("A row update must not erase the screen")
              case char =>
                rows(row) = rows(row).take(column).padTo(column, ' ') + char + rows(row).drop(column + 1)
                column += 1
            remaining = remaining.tail
      assertEquals(rows.take(after.size).toList, after)
      assert(rows.slice(after.size, 11).forall(_.isEmpty))
      assertEquals(rows(11), "footer must remain")
      assertEquals(row, after.size)
    assert(!Screen.replaceRows(List("header", "old"), List("header", "new")).contains("header"))

  test("displayWidth counts wide (CJK) characters as two columns"):
    assertEquals(Screen.displayWidth("abc"), 3)
    assertEquals(Screen.displayWidth("中文"), 4)
    assertEquals(Screen.displayWidth("\u001b[36m$ git status\u001b[0m"), 12) // styles take no cells
