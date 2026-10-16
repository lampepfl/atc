package atc

import atc.host.{CommandLine, OutputBuffer, TextSink}

import java.nio.charset.StandardCharsets
import java.util.concurrent.{CompletableFuture, TimeUnit}
import java.util.regex.Pattern
import scala.jdk.CollectionConverters.*

class ProcessesSuite extends munit.FunSuite:
  test("waiting for output rechecks buffered text before sleeping and wakes on exit"):
    val buffer = OutputBuffer.Tail(64)
    val pattern = Pattern.compile("ready")
    assertEquals(buffer.consumeThrough(pattern), None)
    // Output can arrive after readUntil's first check but before it starts waiting.
    buffer.append("ready\ntail")
    val done = CompletableFuture[Option[String]]()
    val waiter = Thread(() => { done.complete(buffer.awaitMatch(pattern, 10000)); () })
    waiter.setDaemon(true)
    waiter.start()
    try
      assertEquals(done.get(2, TimeUnit.SECONDS), Some("ready"))
      assertEquals(buffer.take(), "\ntail")
    finally
      waiter.interrupt()
      waiter.join(1000)
    buffer.end()
    assertEquals(buffer.awaitMatch(pattern, 10000), None)

  test("captured stderr reports truncation at its output limit"):
    val command = ProcessFixture.command("stderr", (8 * 1024 * 1024 + 1).toString)
    val result = ProcessFixture.run(ProcessBuilder(CommandLine.parseCommandLine(command).asJava), command, 10000L)
    assertEquals(result.exitCode, 0)
    assertEquals(result.stdout, "")
    assert(result.stderr.contains("truncated"), result.stderr.takeRight(100))

  test("tail output coalesces fragmented appends and truncates only past the cap"):
    val buffer = OutputBuffer.Tail(64)
    val text = (0 until 64).map(i => ('a' + i % 26).toChar).mkString
    text.foreach(char => buffer.append(char.toString))

    assertEquals(buffer.peek, text)
    assertEquals(buffer.marker, "")
    assertEquals(buffer.retainedChunkCount, 1)

    buffer.append("!")
    assertEquals(buffer.peek, text.drop(1) + "!")
    assert(buffer.marker.contains("older output dropped: exceeded 64 characters"), buffer.marker)

    val continued = (0 until 4096).map(i => ('A' + i % 26).toChar).mkString
    continued.foreach(char => buffer.append(char.toString))
    assertEquals(buffer.peek, continued.takeRight(64))
    assert(buffer.retainedChunkCount <= 2, s"retained ${buffer.retainedChunkCount} chunks")

  test("tail output preserves consume and take semantics across physical chunks"):
    val chunkSize = 8 * 1024
    val cap = chunkSize + 808
    val buffer = OutputBuffer.Tail(cap)
    buffer.append("a" * chunkSize)
    List("b" * 400, "b" * 408).foreach(buffer.append)
    assertEquals(buffer.retainedChunkCount, 2)

    assertEquals(buffer.consume(chunkSize + 3), "a" * chunkSize + "bbb")
    assertEquals(buffer.peek, "b" * 805)

    buffer.append("c" * (chunkSize + 3))
    assertEquals(buffer.peek, "b" * 805 + "c" * (chunkSize + 3))
    assertEquals(buffer.marker, "")
    assertEquals(buffer.take(), "b" * 805 + "c" * (chunkSize + 3))
    assertEquals(buffer.peek, "")

    buffer.append("d" * (cap + 1))
    assertEquals(buffer.peek, "d" * cap)
    assert(buffer.marker.contains("older output dropped"), buffer.marker)
    assertEquals(buffer.consume(cap + 10), "d" * cap)
    assertEquals(buffer.peek, "")

    buffer.append("tail")
    assertEquals(buffer.take(), "tail")

  test("TextSink incrementally decodes UTF-8, UTF-16 BOMs, and malformed bytes"):
    val sb = StringBuilder()
    val sink = TextSink(s => sb.append(s))
    for b <- "héllo 🙂 x".getBytes("UTF-8") do sink.write(Array(b), 0, 1) // one byte at a time
    sink.finish()
    assertEquals(sb.toString, "héllo 🙂 x")
    val sb2 = StringBuilder()
    val sink2 = TextSink(s => sb2.append(s))
    sink2.write(Array(0xc3.toByte, 0x28.toByte), 0, 2) // malformed UTF-8, not an encoding marker
    sink2.finish()
    assertEquals(sb2.toString, "\uFFFD(")
    def bomText(bom: Array[Byte], encoded: Array[Byte]): String =
      val out = StringBuilder()
      val sink = TextSink(s => out.append(s))
      for byte <- bom ++ encoded do sink.write(Array(byte), 0, 1)
      sink.finish()
      out.toString
    val sample = "bom 🙂"
    assertEquals(
      bomText(Array(0xef.toByte, 0xbb.toByte, 0xbf.toByte), sample.getBytes(StandardCharsets.UTF_8)),
      sample,
    )
    assertEquals(
      bomText(Array(0xff.toByte, 0xfe.toByte), sample.getBytes(StandardCharsets.UTF_16LE)),
      sample,
    )
    assertEquals(
      bomText(Array(0xfe.toByte, 0xff.toByte), sample.getBytes(StandardCharsets.UTF_16BE)),
      sample,
    )
