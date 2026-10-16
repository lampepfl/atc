package atc.ui

import org.jline.terminal.{Size, Sized}
import org.jline.terminal.impl.DumbTerminal

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets.UTF_8

/** The window title and the footer. */
class StatusLineSuite extends munit.FunSuite:
  test("the window title is one line"):
    val out = ByteArrayOutputStream()
    val terminal = DumbTerminal("test", "xterm-256color", ByteArrayInputStream(Array.emptyByteArray), out, UTF_8)
    terminal.setSize(Size.of(80, 24): Sized)
    val screen = Screen(terminal, plain = false, Glyphs.unicode)
    val status = StatusLine(screen, () => false, () => false, () => 0)
    screen.frame(status.setContext("model", "atc · a\nb\tc"))
    assert(out.toString(UTF_8).contains("\u001b]0;atc · a b c\u0007"), out.toString(UTF_8))

  test("a program status report encodes its message on one line"):
    val report = StatusLine.programStatus("blocked", "permission", "Run command\n(sbt test)")
    val msg = java.util.Base64.getEncoder.nn.encodeToString("Run command (sbt test)".getBytes(UTF_8))
    assertEquals(report, s"\u001b]7501;state=blocked:kind=permission:app=atc:msg=$msg\u0007")
    assertEquals(StatusLine.programStatus("idle", "", ""), "\u001b]7501;state=idle:app=atc\u0007")

  test("the program status follows the turn and is sent only when it changes"):
    val out = ByteArrayOutputStream()
    val terminal = DumbTerminal("test", "xterm-256color", ByteArrayInputStream(Array.emptyByteArray), out, UTF_8)
    terminal.setSize(Size.of(80, 24): Sized)
    val screen = Screen(terminal, plain = false, Glyphs.unicode)
    var busy = false
    var popup = false
    val status = StatusLine(screen, () => busy, () => popup, () => 0)
    def reports(): List[String] =
      val sent = "\u001b\\]7501;([^\u0007]*)\u0007".r.findAllMatchIn(out.toString(UTF_8)).map(_.group(1).nn).toList
      out.reset()
      sent
    def encoded(text: String) = java.util.Base64.getEncoder.nn.encodeToString(text.getBytes(UTF_8))

    screen.frame(status.setContext("model", "atc"))
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
    val terminal = org.jline.terminal.impl.ExternalTerminal(
      "test",
      "xterm-256color",
      ByteArrayInputStream(Array.emptyByteArray),
      output,
      UTF_8,
    )
    try
      terminal.setSize(org.jline.terminal.Size.of(80, 24): org.jline.terminal.Sized)
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
