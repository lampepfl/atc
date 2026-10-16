package atc.ui

import org.jline.terminal.{Size, Sized}
import org.jline.terminal.impl.DumbTerminal

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{ConcurrentLinkedQueue, LinkedBlockingQueue}
import scala.jdk.CollectionConverters.*

/** The key reader of a turn: corrections typed or pasted while the agent works,
  * and the escape sequences it reads. */
class KeyReaderSuite extends munit.FunSuite:
  test("a paste reports its text once, when it ends"):
    val input = "\u001b[200~first line\rsecond\u001b[201~"
    val in = ByteArrayInputStream(input.getBytes(UTF_8))
    val terminal = DumbTerminal("test", "xterm-256color", in, ByteArrayOutputStream(), UTF_8)
    terminal.setSize(Size.of(80, 24): Sized)
    val drafts = ConcurrentLinkedQueue[String]()
    val submitted = ConcurrentLinkedQueue[String]()
    val alerts = Alerts(terminal, plain = false, _ => ())
    val keys =
      KeyReader(terminal, plain = false, alerts, () => (), s => { submitted.add(s); () }, s => { drafts.add(s); () })
    keys.start()
    val deadline = System.nanoTime() + 5_000_000_000L
    while !drafts.contains("first line\nsecond") && System.nanoTime() < deadline do Thread.sleep(10)
    keys.stop()
    assert(submitted.isEmpty, "a pasted newline submits nothing")
    assertEquals(drafts.asScala.toList.filter(_.nonEmpty).distinct, List("first line\nsecond"))

  test("a pop-up that reads the end of a paste leaves the key reader out of paste mode"):
    val queue = LinkedBlockingQueue[Int]()
    // Bytes as they are sent, one per read, so a read never waits for more than one.
    val in = new InputStream:
      override def read(): Int = queue.take()
      override def read(bytes: Array[Byte], offset: Int, length: Int): Int =
        if length == 0 then 0
        else
          val next = queue.take()
          if next < 0 then -1
          else
            bytes(offset) = next.toByte
            1
    def send(text: String): Unit = text.getBytes(UTF_8).foreach(b => queue.put(b & 0xff))
    val terminal = DumbTerminal("test", "xterm-256color", in, ByteArrayOutputStream(), UTF_8)
    terminal.setSize(Size.of(80, 24): Sized)
    val drafts = ConcurrentLinkedQueue[String]()
    val submitted = ConcurrentLinkedQueue[String]()
    val alerts = Alerts(terminal, plain = false, _ => ())
    val keys =
      KeyReader(terminal, plain = false, alerts, () => (), s => { submitted.add(s); () }, s => { drafts.add(s); () })
    def await(done: => Boolean): Unit =
      val deadline = System.nanoTime() + 5_000_000_000L
      while !done && System.nanoTime() < deadline do Thread.sleep(10)
    keys.start()
    try
      send("\u001b[200~first")
      await(drafts.contains("first"))
      keys.withPaused:
        send("\u001b[201~")
        val reader = terminal.reader()
        for _ <- 1 to 6 do reader.read(1000L)
      send(" more\r")
      await(!submitted.isEmpty)
    finally
      keys.stop()
      queue.put(-1)
    assertEquals(submitted.asScala.toList, List("first more"))

  test("escape sequences stop at their final byte, timeout or EOF"):
    val expired = org.jline.utils.NonBlockingReader.READ_EXPIRED
    for sequence <- List(
        List('['.toInt, '1'.toInt, ';'.toInt, '5'.toInt, 'A'.toInt),
        List('O'.toInt, 'B'.toInt),
        List('['.toInt, expired),
        List('['.toInt, -1),
        List(expired),
      )
    do
      val input = (sequence :+ 'x'.toInt).iterator
      KeyReader.readEscapeSequence(() => input.next())
      assertEquals(input.next(), 'x'.toInt)

  test("malformed escape sequences cannot retain the terminal reader indefinitely"):
    var reads = 0
    val sequence = KeyReader.readEscapeSequence(() =>
      reads += 1
      if reads == 1 then '['.toInt else ';'.toInt
    )
    assertEquals(reads, 64)
    assertEquals(sequence.length, 64)
