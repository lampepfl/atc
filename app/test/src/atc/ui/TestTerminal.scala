package atc.ui

import org.jline.terminal.{Size, Terminal}
import org.jline.terminal.impl.{DumbTerminal, ExternalTerminal}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8

/** Terminals for the ui suites, reading `input` and writing to `output`. */
object TestTerminal:
  private def nothing: InputStream = ByteArrayInputStream(Array.emptyByteArray)

  /** A dumb terminal of an xterm type. It writes synchronously and sends no probes,
    * whose pending answers would delay the output of a terminal built from streams. */
  def apply(
    width: Int = 80,
    height: Int = 24,
    input: InputStream = nothing,
    output: OutputStream = ByteArrayOutputStream(),
  ): DumbTerminal =
    val terminal = DumbTerminal("test", "xterm-256color", input, output, UTF_8)
    (terminal: Terminal).setSize(Size(width, height))
    terminal

  /** An 80x24 xterm that also supports what a dumb terminal lacks: the line reader's
    * editing and JLine's status bar. */
  def external(input: InputStream = nothing, output: OutputStream = ByteArrayOutputStream()): ExternalTerminal =
    val terminal = ExternalTerminal("test", "xterm-256color", input, output, UTF_8)
    (terminal: Terminal).setSize(Size(80, 24))
    terminal

  /** A screen with Unicode glyphs on a dumb terminal `width` columns wide. */
  def screen(width: Int = 80, output: OutputStream = ByteArrayOutputStream()): Screen =
    Screen(apply(width, output = output), plain = false, Glyphs.unicode)

  /** Input that hands out one byte per read, as `next` gives them, so a read never
    * waits for a second byte; a negative value ends the input. */
  def oneByteAtATime(next: () => Int): InputStream = new InputStream:
    override def read(): Int = next()
    override def read(bytes: Array[Byte], offset: Int, length: Int): Int =
      if length == 0 then 0
      else
        val byte = next()
        if byte < 0 then -1
        else
          bytes(offset) = byte.toByte
          1
