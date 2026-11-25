package atc.ui

import atc.TestEnv

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
    val terminal = TestTerminal.external(ByteArrayInputStream(input.getBytes(UTF_8)), out)
    val screen = Screen(terminal, plain = false, Glyphs.unicode)
    val alerts = Alerts(terminal, plain = false, _ => ())
    (PromptReader(screen, TestEnv.tempDir("atc-prompt").resolve("history").nn, alerts), terminal, out)

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

  test("Shift-Tab cycles the mode at the main prompt only, not in an answer or a secret"):
    // The key map is shared: in the other fields the `/mode` line would be sent as the answer.
    val (prompt, terminal, _) = promptOn("\u001b[Z\r\u001b[Z\r\u001b[Z")
    try
      assertEquals(prompt.readAnswer("instructions> "), None)
      assertEquals(prompt.readSecret("key> "), None)
      assertEquals(prompt.read("> ", ""), PromptReader.CycleModeLine)
    finally terminal.close()

  test("Ctrl-O switches the view and leaves the line being typed alone"):
    // JLine's own binding for the key accepts the line.
    val (prompt, terminal, _) = promptOn("fix the bug in\u000f this\r")
    var toggled = 0
    prompt.onToggle = () => toggled += 1
    try
      assertEquals(prompt.read("> ", ""), "fix the bug in this")
      assertEquals(toggled, 1)
    finally terminal.close()

  /** `text` as a terminal sends a bracketed paste: its line ends as carriage returns. One paste
    * per prompt: JLine 4.7.0 repeats the end of a second paste it reads in one piece with the
    * first (`BindingReader.readStringUntil` keeps the characters it read ahead). */
  private def paste(text: String): String = s"\u001b[200~${text.replace('\n', '\r')}\u001b[201~"
  private val pastedLines = (1 to 12).map(n => s"pasted line $n").mkString("\n")

  test("a long paste shows as a placeholder; the read returns its text, and so does the history"):
    val (prompt, terminal, out) = promptOn(s"see ${paste(pastedLines)} and this\r")
    try
      assertEquals(prompt.read("> ", ""), s"see $pastedLines and this")
      assertEquals(prompt.reader.getHistory.asScala.map(_.line).toList, List(s"see $pastedLines and this"))
      // What the prompt drew: before it, the test terminal echoes the input as it arrives.
      val drawn = out.toString(UTF_8).split("> see ", 2).last
      assert(drawn.contains("[Pasted text #1: 12 lines]"), drawn.replace("\u001b", "ESC"))
      assert(!drawn.contains("pasted line 1"), drawn.replace("\u001b", "ESC"))
    finally terminal.close()

  test("Backspace after a placeholder removes it and its text"):
    val (prompt, terminal, _) = promptOn(s"see ${paste(pastedLines)}\u007f\u007fnothing\r")
    try assertEquals(prompt.read("> ", ""), "seenothing")
    finally terminal.close()

  test("the pastes of one read are not known to the next"):
    val (prompt, terminal, _) = promptOn(s"${paste(pastedLines)}\r[Pasted text #1: 12 lines]\r")
    try
      assertEquals(prompt.read("> ", ""), pastedLines)
      assertEquals(prompt.read("> ", ""), "[Pasted text #1: 12 lines]")
    finally terminal.close()

  test("a short paste is shown as it is"):
    val (prompt, terminal, out) = promptOn(s"see ${paste("line 1\nline 2\nline 3")}\r")
    try
      assertEquals(prompt.read("> ", ""), "see line 1\nline 2\nline 3")
      assert(!out.toString(UTF_8).contains("[Pasted text"), out.toString(UTF_8).replace("\u001b", "ESC"))
    finally terminal.close()

  test("a code block shows a long paste in full"):
    val code = "val a = 1\nval b = 2\nval c = 3\nval d = 4"
    val (prompt, terminal, out) = promptOn(s"${paste(code)}\r\r")
    prompt.blockMode = true
    try
      assertEquals(prompt.read("| ", "").trim, code)
      assert(!out.toString(UTF_8).contains("[Pasted text"), out.toString(UTF_8).replace("\u001b", "ESC"))
    finally terminal.close()

  test("an answer shows a long paste as a placeholder and a secret does not"):
    val (answer, answerTerminal, answerOut) = promptOn(s"${paste(pastedLines)}\r")
    try
      assertEquals(answer.readAnswer("answer> "), Some(pastedLines))
      assert(answerOut.toString(UTF_8).contains("[Pasted text #1: 12 lines]"))
    finally answerTerminal.close()
    val (secret, secretTerminal, secretOut) = promptOn(s"${paste("k" * 900)}\r")
    try
      assertEquals(secret.readSecret("key> "), Some("k" * 900))
      assert(!secretOut.toString(UTF_8).contains("[Pasted text"))
    finally secretTerminal.close()

  test("an open bracket inside pasted code continues a /run, although the placeholder's are closed"):
    val code = "List(1,\n2,\n3,\n4"
    val (prompt, terminal, _) = promptOn(s"/run ${paste(code)}\r)\r")
    try assertEquals(prompt.read("> ", ""), s"/run $code\n  )")
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
    val dir = TestEnv.tempDir("atc-history")
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
    val dir = TestEnv.tempDir("atc-history-link")
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
