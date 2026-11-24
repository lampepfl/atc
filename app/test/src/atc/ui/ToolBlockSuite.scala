package atc.ui

import atc.host.FileChange
import atc.sandbox.ExecutionResult

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.mutable

/** A tool block as the terminal shows it: folded to a summary when it ends in the
  * compact view, written out in full in the expanded one. */
class ToolBlockSuite extends munit.FunSuite:

  /** A block on a 80x24 terminal whose output is kept in the returned stream. */
  private def block(expanded: => Boolean): (Screen, ToolBlock, ByteArrayOutputStream) =
    val out = ByteArrayOutputStream()
    val screen = TestTerminal.screen(output = out)
    (screen, ToolBlock(screen, () => expanded), out)

  /** The rows the output leaves on screen, replaying the cursor moves and line
    * clears a live region uses; styles are dropped. */
  private def rows(out: ByteArrayOutputStream): List[String] =
    val text = out.toString(StandardCharsets.UTF_8)
    val screen = mutable.ArrayBuffer("")
    var row = 0
    var i = 0
    // Parameters, intermediate bytes (`$` in JLine's `ESC[?2026$p` probe), final byte.
    val csi = "\u001b\\[([0-9;?]*)[ -/]*([@-~])".r
    while i < text.length do
      text.charAt(i) match
        case '\u001b' =>
          val m = csi.findPrefixMatchOf(
            text.substring(i)
          ).getOrElse(fail(s"unexpected escape: ${text.substring(i).take(8).drop(1)}"))
          val n = m.group(1).nn.toIntOption.getOrElse(1)
          m.group(2) match
            case "A" => row -= n
            case "B" => row += n
            case "K" => screen(row) = ""
            case _ => () // styles, modes
          i += m.end
        case '\r' => i += 1
        case '\n' =>
          row += 1
          while screen.size <= row do screen += ""
          i += 1
        case c =>
          screen(row) = screen(row) + c
          i += 1
    screen.toList.reverse.dropWhile(_.isEmpty).reverse

  test("a finished block in the compact view leaves only its summary"):
    val (screen, tool, out) = block(expanded = false)
    screen.frame(tool.start("val xs = 1 to 30\nxs.foreach(println)", "run_scala", Nil))
    for i <- 1 to 30 do
      screen.frame(tool.emit(s"line $i\n"))
      assert(rows(out).size <= screen.height - 3, s"the running block must fit above the footer: ${rows(out)}")
    screen.frame(tool.end(ExecutionResult(success = true, output = ""), 12))
    assertEquals(
      rows(out),
      List("● run_scala  val xs = 1 to 30 …", "  └ ok 12 ms · 30 lines of output · /output 1"),
    )

  test("a summary longer than the width is cut to one row, styles and all"):
    val (screen, tool, out) = block(expanded = false)
    val code = "grep(\"app/src/atc/agent/ToolRunner.scala\", \"^(final class|object)\").foreach(m => println(m.line))"
    screen.frame(tool.start(code, "run_scala", Nil))
    screen.frame(tool.end(ExecutionResult(success = true, output = ""), 7))
    val summary = rows(out)
    assertEquals(summary.head, ("● run_scala  " + code).take(78) + "…")
    assertEquals(summary.last, "  └ ok 7 ms · /output 1")

  test("a failed block's summary says why"):
    val (screen, tool, out) = block(expanded = false)
    screen.frame(tool.start("val x: Int = \"s\"", "run_scala", Nil))
    val diagnostic = "-- [E007] Type Mismatch Error: ------\n1 |val x: Int = \"s\"\n  |             ^^^\n"
    screen.frame(tool.end(ExecutionResult(success = false, output = diagnostic), 5))
    assertEquals(
      rows(out),
      List("● run_scala  val x: Int = \"s\"", "  └ failed 5 ms · [E007] Type Mismatch Error · /output 1"),
    )

  test("the expanded view writes the block out in full"):
    val (screen, tool, out) = block(expanded = true)
    screen.frame(tool.start("println(1)", "run_scala", Nil))
    screen.frame(tool.print("1\n", "1\n"))
    screen.frame(tool.end(ExecutionResult(success = true, output = "1\nval r: Int = 1"), 3))
    assertEquals(
      rows(out),
      List(
        "● run_scala",
        "  │ println(1)",
        "  ├ output",
        "  │ 1",
        "  ├ result",
        "  │ val r: Int = 1",
        "  └ ok 3 ms · /output 1",
      ),
    )

  test("the reason a call failed: its error, a coded compiler heading, or the message under a bare one"):
    def problem(output: String, error: Option[String] = None) =
      ToolBlock.firstProblem(ExecutionResult(success = false, output = output, error = error), "")
    assertEquals(
      problem("", Some("java.lang.RuntimeException: boom\n  at x")),
      Some("RuntimeException: boom") // java.lang is noise in a one-line summary
    )
    assertEquals(problem("", Some("java.io.IOException: gone")), Some("java.io.IOException: gone"))
    assertEquals(problem("-- [E007] Type Mismatch Error: ----\n1 |x\n  |^"), Some("[E007] Type Mismatch Error"))
    assertEquals(
      problem("-- Error: ----\n1 |Thread.sleep(1)\n  |^^^^^^^^^^^^\n  |Cannot refer to method sleep\n"),
      Some("Cannot refer to method sleep"),
    )
    assertEquals(problem(""), None)

  test("printed lines that look like stack frames are subtracted as printed"):
    val printed = "running tests\n  at scala.Option.map(Option.scala:1)\n"
    val output = printed + "java.lang.IllegalStateException: boom\n  at rs$line$3$.f(rs$line$3:1)"
    val r = ExecutionResult(success = false, output = output)
    assertEquals(ToolBlock.firstProblem(r, printed), Some("IllegalStateException: boom"))

  test("every line of a classified print is marked"):
    val (screen, tool, out) = block(expanded = true)
    screen.frame(tool.start("println(secret)", "run_scala", Nil))
    screen.frame(tool.print("[classified]\n", "first secret\nsecond secret\n"))
    assertEquals(rows(out).takeRight(2), List("  │ [classified] first secret", "  │ [classified] second secret"))

  test("the expanded view keeps a long output line inside the gutter, across chunks"):
    val (screen, tool, out) = block(expanded = true)
    screen.frame(tool.start("println(long)", "run_scala", Nil))
    screen.frame(tool.emit("x" * 60))
    screen.frame(tool.emit("y" * 60 + "\nshort\n"))
    val shown = rows(out).dropWhile(!_.contains("output")).drop(1)
    assert(shown.take(3).forall(_.startsWith("  │ ")), shown)
    assertEquals(shown.map(_.stripPrefix("  │ ")).take(3).mkString, "x" * 60 + "y" * 60 + "short")
    assert(shown.forall(Screen.displayWidth(_) < 80), shown)

  test("Ctrl-O writes the kept output inside the gutter, and later output starts its rows at the gutter"):
    var expanded = true
    val (screen, tool, out) = block(expanded)
    def toggle(): Unit =
      expanded = !expanded
      screen.frame { tool.detach(); tool.reattach() }
    screen.frame(tool.start("println(long)", "run_scala", Nil))
    screen.frame(tool.emit("abc")) // the expanded view's row now holds three cells
    toggle()
    screen.frame(tool.emit("\n" + "x" * 120 + "\n"))
    toggle()
    screen.frame(tool.emit("y" * 120 + "\n"))
    val shown = rows(out).drop(rows(out).lastIndexWhere(_.contains("output")) + 1)
    assert(shown.forall(row => row.startsWith("  │ ") && Screen.displayWidth(row) < 80), shown)
    val room = 80 - screen.GutterWidth - 1
    assertEquals(shown.map(_.stripPrefix("  │ ")), List("x" * room, "x" * (120 - room), "y" * room, "y" * (120 - room)))

  test("repeated identical file changes fold into one row with a count"):
    val (screen, tool, out) = block(expanded = false)
    screen.frame(tool.start("edits", "run_scala", Nil))
    for _ <- 1 to 4 do tool.fileChanged(atc.host.FileChange("a.py", "updated (+1 -1)", ""))
    tool.fileChanged(atc.host.FileChange("b.py", "created (+3 -0)", ""))
    screen.frame(tool.end(ExecutionResult(success = true, output = ""), 5))
    assertEquals(rows(out).slice(1, 3), List("  a.py: updated (+1 -1), 4 times", "  b.py: created (+3 -0)"))

  test("output that arrives while a pop-up is drawn waits for it to close"):
    val (screen, tool, out) = block(expanded = false)
    screen.frame(tool.start("parallel(tasks)", "run_scala", Nil))
    screen.frame(tool.emit("before\n"))
    screen.frame { tool.endOutput(); tool.hold() }
    val shown = out.size
    screen.frame(tool.emit("during\n"))
    assertEquals(out.size, shown, "nothing is written over the pop-up")
    screen.frame(tool.release())
    assert(rows(out).last.endsWith("during"), rows(out))

  test("withoutPrinted removes the live-shown prints and keeps diagnostics and echoes"):
    val printed = "  leading spaces\nstaged:\nA  file\n"
    val body = "-- Warning: something\n  leading spaces\nstaged:\nA  file\nval r: Int = 1"
    assertEquals(ToolBlock.withoutPrinted(body, printed), "-- Warning: something\nval r: Int = 1")

  test("withoutPrinted handles prints at the start, at the end, and nothing else"):
    assertEquals(ToolBlock.withoutPrinted("hello\nval x: Int = 1", "hello\n"), "val x: Int = 1")
    assertEquals(ToolBlock.withoutPrinted("1 warning found\nhello", "hello\n"), "1 warning found")
    assertEquals(ToolBlock.withoutPrinted("hello", "hello\n"), "")
    assertEquals(ToolBlock.withoutPrinted("only echo", ""), "only echo")

  test("withoutPrinted leaves the body alone when the prints are not found verbatim"):
    val body = "hel...[truncated]"
    assertEquals(ToolBlock.withoutPrinted(body, "hello world\n"), body)

  test("/output writes a retained call as the expanded view writes a block"):
    val (screen, tool, out) = block(expanded = false)
    screen.frame(tool.start("write(\"a.txt\", s)\nprintln(1)", "run_scala", Nil))
    screen.frame(tool.fileChanged(FileChange("a.txt", "updated (+1 -1)", "@@ line 2 @@\n- two\n+ 2")))
    screen.frame(tool.commandOutput("built\n"))
    screen.frame(tool.end(ExecutionResult(success = true, output = "1"), 4))
    out.reset()
    assertEquals(screen.frame(tool.writeRetained(tool.history.latest.get, 1)), 7)
    assertEquals(
      rows(out),
      List(
        "● run_scala  /output 1",
        "  │ write(\"a.txt\", s)",
        "  │ println(1)",
        "  ├ output",
        "  │ 1",
        "  ├ command output",
        "  │ built",
        "  ├ a.txt: updated (+1 -1)",
        "  │ @@ line 2 @@",
        "  │ - two",
        "  │ + 2",
        "  └ ok 4 ms",
      ),
    )

  test("/output pages a long result, repeating the heading of the part a page starts in"):
    val (screen, tool, out) = block(expanded = false)
    screen.frame(tool.start("1 to 300", "run_scala", Nil))
    screen.frame(tool.end(ExecutionResult(success = false, output = (1 to 300).mkString("\n")), 2))
    val entry = tool.history.latest.get
    out.reset()
    assertEquals(screen.frame(tool.writeRetained(entry, 1)), 301)
    val first = rows(out)
    assertEquals(
      first.take(4),
      List("● run_scala  /output 1 · lines 1 to 200 of 301", "  │ 1 to 300", "  ├ error", "  │ 1")
    )
    assertEquals(first.takeRight(2), List("  │ 199", "  └ 101 more lines · /output 1 201"))
    out.reset()
    screen.frame(tool.writeRetained(entry, 201))
    val second = rows(out)
    assertEquals(second.take(3), List("● run_scala  /output 1 · lines 201 to 301 of 301", "  ├ error", "  │ 200"))
    assertEquals(second.takeRight(2), List("  │ 300", "  └ failed 2 ms"))
    out.reset()
    assertEquals(screen.frame(tool.writeRetained(entry, 302)), 301)
    assertEquals(out.size, 0, "nothing is written past the last line")

  test("/output lists the retained calls in columns"):
    val (screen, tool, out) = block(expanded = false)
    screen.frame(tool.start("val x = 1\nx", "run_scala", Nil))
    screen.frame(tool.end(ExecutionResult(success = true, output = "val x: Int = 1"), 12))
    screen.frame(tool.start("oops", "/run", Nil))
    screen.frame(tool.end(ExecutionResult(success = false, output = "Not found: oops"), 7))
    out.reset()
    screen.frame(tool.writeIndex())
    assertEquals(rows(out), List("  1  run_scala  ok     12 ms  val x = 1 …", "  2  /run       failed  7 ms  oops"))

  test("a compiler heading wider than the row has its rule cut, not wrapped"):
    val out = ByteArrayOutputStream()
    val screen = TestTerminal.screen(width = 40, output = out)
    val tool = ToolBlock(screen, () => true)
    screen.frame(tool.start("val x: Int = \"s\"", "run_scala", Nil))
    val diagnostic = "-- [E007] Type Mismatch Error: " + "-" * 50 + "\n1 |val x: Int = \"s\""
    screen.frame(tool.end(ExecutionResult(success = false, output = diagnostic), 5))
    val shown = rows(out).dropWhile(!_.contains("E007"))
    assertEquals(shown.take(2), List("  │ -- [E007] Type Mismatch Error: ----", "  │ 1 |val x: Int = \"s\""))

  test("the expanded view and /output show a call's texts under their names, after the code"):
    val (screen, tool, out) = block(expanded = true)
    screen.frame(tool.start(
      "write(\"a.py\", texts(\"a.py\"))",
      "run_scala",
      List("a.py" -> "def f():\n    return 1\n")
    ))
    screen.frame(tool.end(ExecutionResult(success = true, output = ""), 3))
    val texts = List("  ├ texts(\"a.py\")", "  │ def f():", "  │     return 1")
    assertEquals(rows(out).slice(2, 5), texts)
    out.reset()
    assertEquals(screen.frame(tool.writeRetained(tool.history.latest.get, 1)), 4)
    assertEquals(rows(out).slice(2, 5), texts)

  test("the compact summary of a call with texts is the usual one"):
    val (screen, tool, out) = block(expanded = false)
    screen.frame(tool.start("write(\"a.py\", texts(\"a.py\"))", "run_scala", List("a.py" -> "print(1)\n")))
    screen.frame(tool.end(ExecutionResult(success = true, output = ""), 3))
    assertEquals(rows(out), List("● run_scala  write(\"a.py\", texts(\"a.py\"))", "  └ ok 3 ms · /output 1"))

  test("a text's heading stays on one row whatever its name holds"):
    val (screen, tool, out) = block(expanded = true)
    screen.frame(tool.start("1", "run_scala", List("a\nb\tc" -> "x")))
    assertEquals(rows(out).slice(2, 4), List("  ├ texts(\"a b c\")", "  │ x"))
