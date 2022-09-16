package scala.meta.internal.fastpass.console

import java.io.OutputStream
import java.io.PrintStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.Timer
import java.util.TimerTask

import scala.util.Failure
import scala.util.Try

abstract class Console(val stream: PrintStream, val height: Int)
    extends PrintStream(stream) {
  def display(): Unit

  def apply[T](op: PrintStream => T): Try[T] = Console.withConsole(this)(op)
}

object Console {
  val CSI = "\u001b["
  val ESC = "\u001b"
  private val timer = new Timer( /* isDaemon = */ true)

  def withConsole[T](
      console: Console,
      clear: Boolean = true,
      onError: (Console, Throwable, Path) => Unit = showLogError
  )(op: PrintStream => T): Try[T] = {
    if (System.console() == null) withoutTty(console, onError)(op)
    else withTty(console, clear, onError)(op)
  }

  private def withoutTty[T](
      console: Console,
      onError: (Console, Throwable, Path) => Unit
  )(op: PrintStream => T): Try[T] = {
    val logFile = Files.createTempFile("console-log", null)
    val teeStream = new TeeOutputStream(
      console.stream,
      Files.newOutputStream(logFile)
    )

    val result = Try(op(console.stream))

    result match {
      case Failure(err) => onError(console, err, logFile)
      case _ => Files.deleteIfExists(logFile)
    }

    result
  }

  private def withTty[T](
      console: Console,
      clear: Boolean,
      onError: (Console, Throwable, Path) => Unit
  )(op: PrintStream => T): Try[T] = {
    val task = new TimerTask() {
      override def run(): Unit = {
        erase(console.stream)
        console.stream.flush()
        console.display()
      }
    }

    console.stream.println(System.lineSeparator * (console.height - 1))
    console.stream.print(s"${CSI}${console.height}A${ESC}7")
    timer.scheduleAtFixedRate(task, 0L, 50L)

    val logFile = Files.createTempFile("console-log", null)
    val teeStream = new TeeOutputStream(
      console,
      Files.newOutputStream(logFile)
    )

    val result = Try(op(new PrintStream(teeStream)))
    task.cancel()
    if (clear) erase(console.stream)

    result match {
      case Failure(err) => onError(console, err, logFile)
      case _ => ()
    }

    result
  }

  private def erase(stream: OutputStream): Unit = {
    stream.write(s"${ESC}8${CSI}0J${ESC}7".getBytes("UTF-8"))
  }

  private def showLogError(
      console: Console,
      err: Throwable,
      logFile: Path
  ): Unit = {
    val msg =
      fansi.Color.Red(s"The operation failed. Logs can be found at ${logFile}")
    console.stream.println(msg.render)
  }

  private class TeeOutputStream(o1: OutputStream, o2: OutputStream)
      extends OutputStream {
    override def write(b: Int): Unit = {
      o1.write(b)
      o2.write(b)
    }

    override def flush(): Unit = {
      o1.flush()
      o2.flush()
    }
  }

}
