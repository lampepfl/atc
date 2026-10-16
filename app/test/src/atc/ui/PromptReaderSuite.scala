package atc.ui

import org.jline.terminal.{Size, Sized}
import org.jline.terminal.impl.ExternalTerminal

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The main prompt as the terminal shows it. */
class PromptReaderSuite extends munit.FunSuite:
  /** A prompt on an 80x24 xterm that reads `input`, and what it writes. */
  private def promptOn(input: String): (PromptReader, ExternalTerminal, ByteArrayOutputStream) =
    val out = ByteArrayOutputStream()
    val terminal =
      ExternalTerminal("test", "xterm-256color", ByteArrayInputStream(input.getBytes(UTF_8)), out, UTF_8)
    terminal.setSize(Size.of(80, 24): Sized)
    val screen = Screen(terminal, plain = false, Glyphs.unicode)
    val alerts = Alerts(terminal, plain = false, _ => ())
    (PromptReader(screen, Files.createTempDirectory("atc-prompt").nn.resolve("history").nn, alerts), terminal, out)

  test("Ctrl-L lifts the footer's scroll region and erases its row before clearing"):
    val (prompt, terminal, out) = promptOn("\f\u0004")
    try
      intercept[org.jline.reader.EndOfFileException](prompt.read("> ", ""))
      val text = out.toString(UTF_8)
      val lifted = text.indexOf("\u001b[r\u001b[24;1H\u001b[2K")
      assert(lifted >= 0 && lifted < text.indexOf("\u001b[2J"), text.replace("\u001b", "ESC"))
    finally terminal.close()

  test("the ghost suggestion is not left on the prompt line when the read ends"):
    val (prompt, terminal, out) = promptOn("\u0004")
    prompt.suggestion = Some("rename word_count")
    try
      intercept[org.jline.reader.EndOfFileException](prompt.read("> ", ""))
      // Drawn while the prompt waits, then erased by the redraw that stays on screen, so the
      // transcript does not show a request the user never sent.
      val text = out.toString(UTF_8)
      val ghost = text.lastIndexOf("rename word_count")
      assert(ghost >= 0, text)
      assert(text.indexOf("\u001b[K", ghost) > ghost, text.replace("\u001b", "ESC"))
    finally terminal.close()

  test("completion lists and inserts model names without terminal controls"):
    val (prompt, terminal, out) = promptOn("/model x\t\r")
    prompt.completions = _ => List("x\u001b]0;pwned\u0007y")
    try
      assertEquals(prompt.read("> ", "").trim, "/model x]0;pwnedy")
      assert(!out.toString(UTF_8).contains("\u001b]0;pwned"), out.toString(UTF_8).replace("\u001b", "ESC"))
    finally terminal.close()

  test("answers stay out of the prompt history; prompts go in"):
    val (prompt, terminal, _) = promptOn("my answer\rhello\r")
    try
      assertEquals(prompt.readAnswer("answer> "), Some("my answer"))
      assertEquals(prompt.read("> ", ""), "hello")
      assertEquals(prompt.reader.getHistory.asScala.map(_.line).toList, List("hello"))
    finally terminal.close()

  test("answer and secret fields show no ghost text, and → or Tab does not insert it"):
    val (prompt, terminal, out) = promptOn("\u001b[C\r\t\r")
    prompt.suggestion = Some("rename word_count")
    try
      assertEquals(prompt.readSecret("key> "), None)
      assertEquals(prompt.readAnswer("answer> "), None)
      assert(!out.toString(UTF_8).contains("rename word_count"), out.toString(UTF_8).replace("\u001b", "ESC"))
    finally terminal.close()
