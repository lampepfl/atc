package atc.ui

/** The placeholders that stand for long pastes at the prompt. */
class PastedTextSuite extends munit.FunSuite:
  private val fourLines = "a\nb\nc\nd"

  test("a paste is long with more than three lines or more than 800 characters"):
    assert(!PastedText.isLong("a\nb\nc"))
    assert(PastedText.isLong(fourLines))
    assert(!PastedText.isLong("x" * 800))
    assert(PastedText.isLong("x" * 801))

  test("a placeholder gives the paste's number and its lines, or its characters on one line"):
    assertEquals(PastedText.placeholder(1, fourLines), "[Pasted text #1: 4 lines]")
    assertEquals(PastedText.placeholder(2, "x" * 801), "[Pasted text #2: 801 chars]")
    // Characters, not UTF-16 units.
    assertEquals(PastedText.placeholder(3, "😀" * 801), "[Pasted text #3: 801 chars]")

  test("short pastes get no placeholder; long ones are numbered and expand to their text"):
    val pasted = PastedText()
    assertEquals(pasted.collapse("short"), None)
    assertEquals(pasted.pattern, None)
    assertEquals(pasted.collapse(fourLines), Some("[Pasted text #1: 4 lines]"))
    assertEquals(pasted.collapse("x" * 900), Some("[Pasted text #2: 900 chars]"))
    assertEquals(
      pasted.expand("see [Pasted text #2: 900 chars] and [Pasted text #1: 4 lines], twice: [Pasted text #1: 4 lines]"),
      s"see ${"x" * 900} and $fourLines, twice: $fourLines",
    )

  test("an edited placeholder and a line without any stay as they read"):
    val pasted = PastedText()
    assertEquals(pasted.expand("[Pasted text #1: 4 lines]"), "[Pasted text #1: 4 lines]")
    pasted.collapse(fourLines)
    assertEquals(pasted.expand("[Pasted text #1: 4 line]"), "[Pasted text #1: 4 line]")
    pasted.clear()
    assertEquals(pasted.expand("[Pasted text #1: 4 lines]"), "[Pasted text #1: 4 lines]")

  test("pasted text that reads as a placeholder, or holds `$` or `\\`, is returned as pasted"):
    val pasted = PastedText()
    val first = pasted.collapse(fourLines).get
    val tricky = s"$first\n$$1 \\n\nc\nd"
    val second = pasted.collapse(tricky).get
    assertEquals(pasted.expand(second), tricky)

  test("endingAt finds the placeholder that ends at the cursor"):
    val pasted = PastedText()
    val placeholder = pasted.collapse(fourLines).get
    val line = s"see $placeholder now"
    assertEquals(pasted.endingAt(line, 4 + placeholder.length), Some(placeholder))
    assertEquals(pasted.endingAt(line, 3 + placeholder.length), None)
    assertEquals(pasted.endingAt(line, line.length), None)
    assertEquals(pasted.endingAt(line, 2), None)
