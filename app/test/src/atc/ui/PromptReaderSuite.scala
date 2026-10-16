package atc.ui

import atc.TestEnv

import org.jline.terminal.{Size, Sized}
import org.jline.terminal.impl.ExternalTerminal

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The main prompt: what the terminal shows, its history file and its command list. */
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

  test("cancelling a text answer clears JLine's interrupt before returning to the menu"):
    try
      val answer = PromptReader.answerOf {
        Thread.currentThread().interrupt()
        throw org.jline.reader.UserInterruptException("unfinished")
      }
      assertEquals(answer, None)
      assert(!Thread.currentThread().isInterrupted)
      assertEquals(PromptReader.answerOf(" revised instructions "), Some("revised instructions"))
      assertEquals(PromptReader.answerOf(throw org.jline.reader.EndOfFileException()), None)
    finally Thread.interrupted()

  test("history is an owner-only regular file where POSIX permissions exist"):
    val dir = Files.createTempDirectory("atc-history").nn
    val history = dir.resolve("nested/history").nn
    assertEquals(PromptReader.secureHistoryFile(history), history.toRealPath())
    assert(Files.isRegularFile(history))
    val view = Files.getFileAttributeView(history, classOf[java.nio.file.attribute.PosixFileAttributeView])
    if view != null then
      val permissive = java.nio.file.attribute.PosixFilePermissions.fromString("rw-r--r--")
      Files.setPosixFilePermissions(history, permissive)
      PromptReader.secureHistoryFile(history)
      val perms = Files.getPosixFilePermissions(history).nn
      assertEquals(perms, java.nio.file.attribute.PosixFilePermissions.fromString("rw-------"))

  test("history refuses a final symbolic link"):
    val dir = Files.createTempDirectory("atc-history-link").nn
    val target = dir.resolve("target").nn
    Files.writeString(target, "do not append here")
    val link = dir.resolve("history-link").nn
    assume(TestEnv.trySymbolicLink(link, target), "symbolic links are unavailable for this account")
    val e = intercept[IllegalArgumentException](PromptReader.secureHistoryFile(link))
    assert(e.getMessage.nn.contains("symbolic link"), e.getMessage)
    assertEquals(Files.readString(target), "do not append here")

  private def commandRows =
    List("/model [ref]" -> "choose a model", "/models" -> "list models", "/mode [name]" -> "change mode")

  test("matchingCommands: a single word starting with / lists the names it starts, case-insensitively"):
    assertEquals(PromptReader.matchingCommands("/", commandRows), commandRows)
    assertEquals(
      PromptReader.matchingCommands("/MODE", commandRows).map(_._1),
      List("/model [ref]", "/models", "/mode [name]")
    )
    assertEquals(PromptReader.matchingCommands("/models", commandRows).map(_._1), List("/models"))
    assertEquals(PromptReader.matchingCommands("/x", commandRows), Nil)
    assertEquals(PromptReader.matchingCommands("/model ", commandRows), Nil) // arguments: Tab completion instead
    assertEquals(PromptReader.matchingCommands("/mo\nx", commandRows), Nil)
    assertEquals(PromptReader.matchingCommands("model", commandRows), Nil)
    assertEquals(PromptReader.matchingCommands("", commandRows), Nil)

  test("renderCommands: aligned rows cut to the width, scrolled to keep the selection in view"):
    def plain(selected: Int, width: Int, height: Int) =
      PromptReader.renderCommands(commandRows, selected, ">", width, height).toString.split("\n").toList
    assertEquals(
      plain(0, 80, 5),
      List("> /model [ref]  choose a model", "  /models       list models", "  /mode [name]  change mode"),
    )
    assertEquals(plain(2, 80, 2), List("  /models       list models", "> /mode [name]  change mode"))
    assertEquals(plain(0, 80, 2).head, "> /model [ref]  choose a model")
    assert(plain(1, 12, 5).forall(_.length <= 11))
