package scala.meta.internal.fastpass.console

import java.io.ByteArrayOutputStream
import java.io.OutputStream
import java.io.PrintStream

class ScrollableConsole(stream: PrintStream, height: Int)
    extends Console(stream, height) {

  private val lines = new CircularBuffer(height)
  private[this] var toErase: Int = 0

  override def write(b: Int): Unit = lines.append(b)

  override def flush(): Unit = stream.flush()

  override def display(): Unit = {
    lines.flushTo(stream)
  }
}

private class CircularBuffer(bufSize: Int) {

  private val entries: Array[ByteArrayOutputStream] =
    Array.fill(bufSize)(new ByteArrayOutputStream)
  private var currentStream: Int = 0
  private var lines: Int = 0

  def append(b: Int): Unit = {
    if (lines == 0) lines = 1
    if (b == 10) push()
    else entries(currentStream).write(b)
  }

  def flushTo(out: OutputStream): Unit = {
    foreach { buffer =>
      buffer.writeTo(out)
      out.write(10)
    }
  }

  private def push(): Unit = {
    lines = Math.min(bufSize, lines + 1)
    currentStream = (currentStream + 1) % bufSize
    if (lines == bufSize) {
      entries(currentStream).reset()
    }
  }

  override def toString: String = {
    val output = new StringBuilder()
    foreach { buffer =>
      output.append(buffer.toString("UTF-8")).append(System.lineSeparator())
    }
    output.toString()
  }

  private def foreach(op: ByteArrayOutputStream => Unit): Unit = {
    (0 until lines).foreach { idx =>
      val bufferIdx =
        if (lines < bufSize) idx
        else (currentStream + lines + idx + 1) % bufSize
      op(entries(bufferIdx))
    }
  }
}
