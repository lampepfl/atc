package atc.ui

import atc.TestEnv

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets

/** The terminal front-end on a non-interactive terminal, and the helpers no other
  * ui suite owns: formatting, sanitizing, the tail buffer and multi-line input. */
class TuiSuite extends munit.FunSuite:

  test("glyphs fall back to ASCII when a dumb terminal has no encoding"):
    assertEquals(Tui.glyphs(null, forceAscii = false), Glyphs.ascii)
    assertEquals(Tui.glyphs(StandardCharsets.UTF_8, forceAscii = false), Glyphs.unicode)
    assertEquals(Tui.glyphs(StandardCharsets.UTF_8, forceAscii = true), Glyphs.ascii)

  test("non-interactive terminals are explicitly dumb UTF-8 terminals"):
    val terminal = Tui.openTerminal(
      nonInteractive = true,
      input = ByteArrayInputStream(Array.emptyByteArray),
      output = ByteArrayOutputStream(),
    )
    try
      assertEquals(terminal.getType, org.jline.terminal.Terminal.TYPE_DUMB)
      assertEquals(terminal.encoding(), StandardCharsets.UTF_8)
    finally terminal.close()

  test("a process event during streaming prose waits for the end of the turn, on a line of its own"):
    val out = ByteArrayOutputStream()
    val saved = System.out
    // The non-interactive terminal writes to the `System.out` of when it is built.
    System.setOut(java.io.PrintStream(out, true, StandardCharsets.UTF_8))
    val tui =
      try Tui(TestEnv.tempDir("atc-tui").resolve("history").nn, nonInteractive = true)
      finally System.setOut(saved)
    try
      tui.beginTurn()
      tui.assistantDelta("Hello ")
      tui.processEvent("[p1 exited 0]")
      tui.assistantDelta("world")
      tui.endTurn()
      val lines = out.toString(StandardCharsets.UTF_8).linesIterator.toList
      assert(lines.exists(_.endsWith(" Hello world")), lines)
      assert(lines.contains("[p1 exited 0]"), lines)
    finally tui.close()

  test("a TODO status change shows the items that changed and the progress, a new list the whole list"):
    val out = ByteArrayOutputStream()
    val saved = System.out
    System.setOut(java.io.PrintStream(out, true, StandardCharsets.UTF_8))
    val tui =
      try Tui(TestEnv.tempDir("atc-tui").resolve("history").nn, nonInteractive = true)
      finally System.setOut(saved)
    try
      import atc.lib.{Todo, TodoStatus}
      val plan = List(Todo("read"), Todo("fix"), Todo("test"))
      def shown(todos: List[Todo]): List[String] =
        out.reset()
        tui.beginTurn()
        tui.showTodos(todos)
        tui.endTurn() // draws what is pending
        out.toString(StandardCharsets.UTF_8).linesIterator.map(_.trim).filter(_.nonEmpty).toList
      assertEquals(shown(plan).count(_.contains(" read")) + shown(Nil).size, 2) // whole list; then empty
      shown(plan)
      val update = shown(plan.updated(1, Todo("fix", TodoStatus.Done)))
      assert(update.exists(_.contains("1 of 3 done")), update)
      assert(update.exists(_.endsWith("fix")) && !update.exists(_.endsWith("read")), update)
      assertEquals(shown(plan.updated(1, Todo("fix", TodoStatus.Done))), Nil) // nothing changed
      assert(shown(plan :+ Todo("ship")).exists(_.endsWith("read")), "a new list is shown whole")
    finally tui.close()

  test("count: short forms, without a pointless .0"):
    assertEquals(Format.count(999), "999")
    assertEquals(Format.count(1234), "1.2k")
    assertEquals(Format.count(200_000), "200k")
    assertEquals(Format.count(1_000_000), "1M")
    assertEquals(Format.count(1_234_567), "1.2M")

  test("contextUsage: against the window when known, an estimate otherwise"):
    assertEquals(Format.contextUsage(45_200, Some(200_000)), "context 45.2k/200k (23%)")
    assertEquals(Format.contextUsage(199_000, Some(200_000)), "context 199k/200k (100%)")
    assertEquals(Format.contextUsage(45_200, None), "context ~45.2k")

  // ── sanitization, widths, durations, the tail buffer ─────────────

  test("sanitize strips terminal control, keeps text, newlines and tabs"):
    val esc = 27.toChar.toString
    assertEquals(Ansi.sanitize("plain text\nmore"), "plain text\nmore")
    assertEquals(Ansi.sanitize("a" + 13.toChar + "b"), "ab") // a bare CR cannot reset the column
    assertEquals(Ansi.sanitize("a\tb\nc"), "a\tb\nc")
    assertEquals(Ansi.sanitize("unicode: héllo 中文"), "unicode: héllo 中文")
    // an injected clear-screen / OSC-52 clipboard write loses its ESC byte and goes inert
    assertEquals(Ansi.sanitize("before" + esc + "[2Jafter"), "before[2Jafter")
    assertEquals(Ansi.sanitize("x" + esc + "]52;c;eGk=" + 7.toChar + "y"), "x]52;c;eGk=y")
    assertEquals(Ansi.sanitize("c1: " + 0x85.toChar), "c1: ")
    assertEquals(Ansi.sanitize("del: " + 0x7f.toChar), "del: ")
    assertEquals(Ansi.sanitize("safe\u202eevil\u202c.txt"), "safeevil.txt")
    assertEquals(Ansi.sanitize("a\u2066b\u2069c\u200fd"), "abcd")
    assertEquals(Ansi.sanitize(""), "")

  test("duration never prints 60 seconds"):
    assertEquals(Format.duration(119.6), "2 min 0 s") // the seconds round up into the minute, not to "1 min 60 s"
    assertEquals(Format.duration(65.4), "1 min 5 s")
    assertEquals(Format.duration(60.0), "1 min 0 s")

  test("TailBuffer: the tail is the last n lines; a trailing newline is not a line"):
    val b = TailBuffer(1000)
    b.append("a\nb\nc")
    assertEquals(b.tail(2), List("b", "c")) // the unfinished last line counts
    assertEquals(b.lineCount, 3L)
    b.append("d\ne\n")
    assertEquals(b.tail(2), List("cd", "e"))
    assertEquals(b.lineCount, 4L)
    b.append("f\n")
    assertEquals(b.tail(10), List("a", "b", "cd", "e", "f"))

  test("TailBuffer: past half again the cap the front goes, the counts stay exact"):
    val b = TailBuffer(10)
    b.append("01234\n67890\n")
    assertEquals(b.text, "01234\n67890\n", "cut only once it grows past 15")
    b.append("abcd\n")
    assertEquals(b.text, "abcd\n")
    assertEquals(b.lineCount, 3L) // the dropped lines still count
    assertEquals(b.tail(5), List("abcd"))
    assertEquals(b.tail(0), Nil)
    b.append("unfinished")
    assertEquals(b.tail(0), Nil)

  test("TailBuffer: cutting in batches keeps the tails the live views show"):
    // A reference that cuts back to the cap on every append.
    final class EveryAppend(cap: Int):
      val sb = StringBuilder()
      def append(text: String): Unit =
        sb.append(text)
        if sb.length > cap then
          val nl = sb.indexOf("\n", sb.length - cap)
          sb.delete(0, if nl >= 0 then nl + 1 else sb.length - cap)
      def tail(n: Int): List[String] = sb.toString.split("\n", -1).toList match
        case init :+ "" => init.takeRight(n)
        case ls => ls.takeRight(n)
    // Both keep the whole lines of about the last `cap` characters, so the last ten short lines agree.
    val random = scala.util.Random(7)
    val batched = TailBuffer(1000)
    val reference = EveryAppend(1000)
    for _ <- 1 to 20000 do
      val chunk = random.alphanumeric.take(random.nextInt(12)).mkString + (if random.nextInt(3) == 0 then "\n" else "")
      batched.append(chunk)
      reference.append(chunk)
      assert(batched.text.length <= 1500)
      for n <- 1 to 10 do assertEquals(batched.tail(n), reference.tail(n))

  // ── multi-line input (Continuation) ───────────────────────────────

  import Continuation.{pending, unclosed}

  test("unclosed: brackets nest, innermost first; balanced code leaves nothing open"):
    assertEquals(unclosed("foo(a, List(1"), List(")", ")"))
    assertEquals(unclosed("def f = { x.map { y => (y, "), List(")", "}", "}"))
    assertEquals(unclosed("foo(List(1, 2)).map(_ + 1)"), Nil)
    assertEquals(unclosed("1 + 1"), Nil)
    assertEquals(unclosed("Map(1 -> 2)))"), Nil) // a stray closer is not waited on

  test("unclosed: strings, char literals and comments hide brackets"):
    assertEquals(unclosed("println(\"a (b\")"), Nil)
    assertEquals(unclosed("\"(\\\"\" + x"), Nil) // an escaped quote does not close the string
    assertEquals(unclosed("println(\"abc"), List("\"", ")"))
    assertEquals(unclosed("println(\"abc\nfoo)"), Nil) // an unterminated string ends at its line
    assertEquals(unclosed("\"\"\"a ( \" b"), List("\"\"\""))
    assertEquals(unclosed("\"\"\"a\"\"\"\" + (1"), List(")")) // `""""` closes it
    assertEquals(unclosed("List('(', '\\'', 'x')"), Nil)
    assertEquals(unclosed("x // a comment with (\nfoo"), Nil)
    assertEquals(unclosed("/* a ( comment"), List("*/"))
    assertEquals(unclosed("/* nested /* ( */ still open"), List("*/"))
    assertEquals(unclosed("/* ( */ ok(1)"), Nil)

  test("pending: a /run continues while brackets are open, indented by their depth; other input does not"):
    assertEquals(pending("/run foo(List(1,", block = false), Some(2))
    assertEquals(pending("/RUN x.map { y =>", block = false), Some(1))
    assertEquals(pending("/scala \"\"\"multi", block = false), Some(0)) // a string: continue, no indent
    assertEquals(pending("/run 1 + 1", block = false), None)
    assertEquals(pending("please fix foo(", block = false), None) // a request, not code
    assertEquals(pending("/mode local", block = false), None)

  test("pending: an empty last line always submits"):
    assertEquals(pending("/run foo(\n  ", block = false), None) // Enter on the (indented) empty line: submit anyway
    assertEquals(pending("/run foo(\nbar", block = false), Some(1))
    assertEquals(pending("first line \\", block = false), None) // a backslash is a key sequence, not a parse rule

  test("pending: block mode continues until an empty line"):
    assertEquals(pending("val x = 1", block = true), Some(0))
    assertEquals(pending("val x = 1\nx + 1", block = true), Some(0))
    assertEquals(pending("val x = 1\n", block = true), None)
    assertEquals(pending("", block = true), None)
