package atc.ui

import atc.TestEnv
import atc.perms.{Decision, ExecRequest}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.nio.charset.StandardCharsets.UTF_8

/** Pop-ups as the user meets them: what they show and what a key does. */
class DialogsSuite extends munit.FunSuite:

  /** Dialogs on a `width`-column terminal that sends `input` and then ends; what it wrote. */
  private def dialogs(input: String, width: Int = 80, plain: Boolean = false): (Dialogs, ByteArrayOutputStream) =
    dialogsReading(ByteArrayInputStream(input.getBytes(UTF_8)), width, plain)

  private def dialogsReading(source: InputStream, width: Int, plain: Boolean): (Dialogs, ByteArrayOutputStream) =
    val out = ByteArrayOutputStream()
    val terminal = TestTerminal(width, input = source, output = out)
    val screen = Screen(terminal, plain, Glyphs.unicode)
    val alerts = Alerts(terminal, plain = false, _ => ())
    val status = StatusLine(screen, () => true, () => true, () => 0)
    val prompt = PromptReader(screen, TestEnv.tempDir("atc-dialogs").resolve("history").nn, alerts)
    (Dialogs(screen, alerts, status, prompt), out)

  private def written(out: ByteArrayOutputStream): String = Ansi.Sgr.replaceAllIn(out.toString(UTF_8), "")

  /** Input sent in parts, each after its delay: a menu ignores an Enter that comes just after it opens. */
  private def paced(parts: (Long, String)*): InputStream =
    val pending = parts.iterator
    var current = ByteArrayInputStream(Array.emptyByteArray)
    TestTerminal.oneByteAtATime: () =>
      var next = current.read()
      while next < 0 && pending.hasNext do
        val (delay, text) = pending.next()
        Thread.sleep(delay)
        current = ByteArrayInputStream(text.getBytes(UTF_8))
        next = current.read()
      next

  test("a stray Enter does not approve a permission request, and Esc is shown to deny"):
    val (d, out) = dialogs("\r")
    assertEquals(d.permission(ExecRequest(List("rm -rf build"), "clean the build")), Decision.Deny)
    assert(written(out).contains("Esc deny"), written(out))

  test("a long question wraps with its lines under the first one's text"):
    val question = "Which of the three build directories should be removed before the next release is cut?"
    val (d, out) = dialogs("", width = 40)
    assertEquals(d.answer(question, List("all", "none"), multiple = false), None)
    val lines = TextLayout.wrap(question, 35)
    assert(lines.size > 1)
    val expected = (("  ? " + lines.head) :: lines.tail.map("    " + _)).mkString("\n")
    assert(written(out).contains(expected), written(out))

  test("a request too long for the window says so and opens on the denial"):
    // Its first rows, the title among them, have scrolled out of sight: Enter must not allow them.
    val many = ExecRequest((1 to 40).map(i => s"tool$i --flag").toList, "run the build")
    val (long, out) = dialogsReading(paced(500L -> "\r"), 80, plain = false)
    assertEquals(long.permission(many), Decision.Deny)
    assert(written(out).contains("the first are above this window"), written(out))
    val (short, shortOut) = dialogsReading(paced(500L -> "\r"), 80, plain = false)
    assertEquals(short.permission(ExecRequest(List("npm test"), "run the tests")), Decision.AllowOnce)
    assert(!written(shortOut).contains("above this window"), written(shortOut))

  test("after empty instructions the permission menu opens again on that row, not on Allow once"):
    // Up from the first row reaches the last, the instructions; Enter with nothing typed goes back
    // to the menu, where Enter must not allow the request.
    val input = paced(700L -> "\u001b[A\r\r", 700L -> "\r", 0L -> "only run the tests\r")
    val (d, _) = dialogsReading(input, width = 80, plain = false)
    assertEquals(d.permission(ExecRequest(List("npm test"), "run the tests")), Decision.Revise("only run the tests"))

  test("ticked answers stay when the added answer is left empty"):
    // Tick the first, move to "Add an answer", tick it, save, then Enter on an empty line.
    val input = paced(700L -> " \u001b[B\u001b[B ", 400L -> "\r", 300L -> "\r")
    val (d, _) = dialogsReading(input, width = 80, plain = false)
    assertEquals(d.answer("Which?", List("a", "b"), multiple = true), Some("a"))

  test("a line of setup text is read under its question, and empty means none"):
    val (d, out) = dialogs("http://localhost:11434/v1\r")
    assertEquals(d.text("Its base URL"), Some("http://localhost:11434/v1"))
    assert(written(out).contains("? Its base URL"), written(out))
    val (empty, _) = dialogs("\r")
    assertEquals(empty.text("A name for it"), None)

  test("a typed confirmation takes only y or yes"):
    for (reply, expected) <- List("y" -> true, "YES" -> true, "yes, but later" -> false, "yolo" -> false, "" -> false)
    do
      val (d, _) = dialogs(s"$reply\r", plain = true)
      assertEquals(d.confirm("Trust this project's config?"), expected, reply)

  test("plain permission prompts require an exact approval and preserve qualified answers as instructions"):
    for answer <- List("y", "YES", " yes ") do
      assertEquals(Menus.permissionReply(Some(answer)), Decision.AllowOnce)
    for answer <- List("s", "SESSION", " session ") do
      assertEquals(Menus.permissionReply(Some(answer)), Decision.AllowSession)
    for answer <- List(None, Some(""), Some("  "), Some("n"), Some("NO")) do
      assertEquals(Menus.permissionReply(answer), Decision.Deny)
    for answer <- List("yes, except the fifth command", "skip deployment", "session only for tests", "只运行测试") do
      assertEquals(Menus.permissionReply(Some(answer)), Decision.Revise(answer))
