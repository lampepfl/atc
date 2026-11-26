package atc.ui

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8

/** The window title and the footer. */
class StatusLineSuite extends munit.FunSuite:
  test("the window title is one line"):
    val out = ByteArrayOutputStream()
    val screen = TestTerminal.screen(output = out)
    val status = StatusLine(screen, () => false, () => false, () => 0)
    screen.frame(status.setTitle(Some("atc · a\nb\tc")))
    assert(out.toString(UTF_8).contains("\u001b]0;atc · a b c\u0007"), out.toString(UTF_8))

  test("the title also names the tmux window, when it changes; without one, neither is touched"):
    val out = ByteArrayOutputStream()
    val screen = TestTerminal.screen(output = out)
    val named = scala.collection.mutable.Buffer[String]()
    val run: List[String] => Option[String] =
      case "display-message" :: _ => Some("1")
      case "rename-window" :: arguments => named += arguments.last; Some("")
      case _ => Some("")
    var busy = false
    val status = StatusLine(screen, () => busy, () => false, () => 0, Some(TmuxWindow("%1", run, _.run())))
    def titles(): List[String] =
      val sent = "\u001b\\]0;([^\u0007]*)\u0007".r.findAllMatchIn(out.toString(UTF_8)).map(_.group(1).nn).toList
      out.reset()
      sent

    // Before the config says whether the tab is named, and when it says no.
    screen.frame(status.setContext("model"))
    screen.frame(status.setTitle(None))
    assertEquals(titles(), Nil)
    assertEquals(named.toList, Nil)
    screen.frame(status.setTitle(Some("atc · project")))
    screen.frame(status.refresh())
    busy = true
    screen.frame(status.beginTurn())
    screen.frame(status.setOperation("reasoning"))
    busy = false
    screen.frame(status.setTitle(Some("my tab")))
    val expected = List("atc · project", "● atc · project", "my tab")
    assertEquals(titles(), expected)
    assertEquals(named.toList, expected)

  test("a terminal that cannot draw a footer gets none, so the spinner shows progress"):
    val vt52 = org.jline.terminal.impl.DumbTerminal(
      "test",
      "vt52",
      java.io.ByteArrayInputStream(Array.emptyByteArray),
      ByteArrayOutputStream(),
      UTF_8
    )
    val screen = Screen(vt52, plain = false, Glyphs.unicode)
    assert(!StatusLine(screen, () => false, () => false, () => 0).shown)
    assert(StatusLine(TestTerminal.screen(), () => false, () => false, () => 0).shown)

  test("the footer names the live processes and monitors after the context, when there are any"):
    val status = StatusLine(TestTerminal.screen(), () => false, () => false, () => 0)
    status.setContext("full · echo · proj")
    assertEquals(status.label, "full · echo · proj")
    status.background = (2, 1)
    assertEquals(status.label, "full · echo · proj · 2 processes · 1 monitor")
    status.background = (1, 0)
    assertEquals(status.label, "full · echo · proj · 1 process")
    status.background = (0, 3)
    assertEquals(status.label, "full · echo · proj · 3 monitors")

  test("a program status report encodes its message on one line"):
    val report = StatusLine.programStatus("blocked", "permission", "Run command\n(sbt test)")
    val msg = java.util.Base64.getEncoder.nn.encodeToString("Run command (sbt test)".getBytes(UTF_8))
    assertEquals(report, s"\u001b]7501;state=blocked:kind=permission:app=atc:msg=$msg\u0007")
    assertEquals(StatusLine.programStatus("idle", "", ""), "\u001b]7501;state=idle:app=atc\u0007")

  test("the program status follows the turn and is sent only when it changes"):
    val out = ByteArrayOutputStream()
    val screen = TestTerminal.screen(output = out)
    var busy = false
    var popup = false
    val status = StatusLine(screen, () => busy, () => popup, () => 0)
    def reports(): List[String] =
      val sent = "\u001b\\]7501;([^\u0007]*)\u0007".r.findAllMatchIn(out.toString(UTF_8)).map(_.group(1).nn).toList
      out.reset()
      sent
    def encoded(text: String) = java.util.Base64.getEncoder.nn.encodeToString(text.getBytes(UTF_8))

    screen.frame(status.setContext("model"))
    assertEquals(reports(), List("state=idle:app=atc"))
    busy = true
    screen.frame(status.beginTurn())
    screen.frame(status.refresh())
    assertEquals(reports(), List(s"state=working:app=atc:msg=${encoded("starting turn")}"))
    popup = true
    status.waitingFor = ("permission", "Run command")
    screen.frame(status.refreshTitle())
    assertEquals(reports(), List(s"state=blocked:kind=permission:app=atc:msg=${encoded("Run command")}"))
    popup = false
    busy = false
    screen.frame:
      status.turnEnded(Some(atc.agent.TurnOutcome.Finished -> "All tests pass"))
      status.refresh()
    assertEquals(reports(), List(s"state=done:app=atc:msg=${encoded("All tests pass")}"))
    screen.frame:
      status.turnEnded(Some(atc.agent.TurnOutcome.Interrupted -> "Interrupted after 3s"))
      status.refresh()
    assertEquals(reports(), List("state=idle:app=atc"))
    status.close()
    assertEquals(reports(), List("state=clear"))

  test("the footer draw leaves the terminal outside a synchronized update (JLine buffers the closer)"):
    val output = ByteArrayOutputStream()
    val terminal = TestTerminal.external(output = output)
    try
      val status = org.jline.utils.Status.getStatus(terminal)
      assert(status != null)
      StatusLine.draw(terminal, status, "ready")
      StatusLine.draw(terminal, status, "reasoning 1.0 s")
      val written = output.toString(UTF_8)
      def count(sub: String): Int = written.sliding(sub.length).count(_ == sub)
      val begins = count("\u001b[?2026h")
      val ends = count("\u001b[?2026l")
      assertEquals(begins, 2)
      assertEquals(ends, 2)
      assert(written.lastIndexOf("\u001b[?2026l") > written.lastIndexOf("\u001b[?2026h"))
      assert(written.contains("ready")) // the second draw only rewrites the changed suffix
    finally terminal.close()
