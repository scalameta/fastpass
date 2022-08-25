package scala.meta.internal.fastpass.console

import java.io.PrintStream

import scala.collection.GenTraversableLike
import scala.collection.generic.CanBuildFrom
import scala.util.Failure
import scala.util.Try
import scala.util.control.NonFatal

import scala.meta.internal.fastpass.Time
import scala.meta.internal.fastpass.Timer

class ProgressConsole(progress: Progress, title: String, output: Console)
    extends Console(output.stream, output.height + 1) {
  private var currentProgress: ProgressUpdate = new ProgressUpdate.Spinner()

  def setProgress(update: ProgressUpdate): Unit = currentProgress = update

  override def write(x: Int): Unit = {
    progress.consume(x).foreach(setProgress)
    output.write(x)
  }

  override def display(): Unit = {
    output.println(currentProgress.render(10) + " " + title)
    output.display()
  }
}

object ProgressConsole {

  def map[Repr[T] <: GenTraversableLike[T, Repr[T]], T, B, That](
      title: String,
      elems: Repr[T]
  )(
      f: T => B
  )(implicit bf: CanBuildFrom[Repr[T], B, That]): Try[That] = {
    val builder = bf()
    foreach(title, elems) { elem =>
      builder += f(elem)
    }.map(_ => builder.result())
  }

  def foreach[Repr[T] <: GenTraversableLike[T, Repr[T]], T](
      title: String,
      elems: Repr[T]
  )(f: T => Unit): Try[Unit] = {
    if (!elems.hasDefiniteSize) auto(title)(_ => elems.foreach(f))
    else {
      manual(title) { advance =>
        val total = elems.size
        elems.foreach { elem =>
          f(elem)
          advance(1, total)
        }
      }
    }
  }

  def auto[T](
      title: String,
      output: Console = new ScrollableConsole(System.err, 5),
      progress: Progress = Progress.NoProgress
  )(op: PrintStream => T): Try[T] = {
    val console = new ProgressConsole(progress, title, output)
    timed(output.stream, title, console(op))
  }

  def manual[T](
      title: String,
      output: Console = new ScrollableConsole(System.err, 5)
  )(op: ((Long, Long) => Unit) => T): Try[T] = {
    val console = new ProgressConsole(Progress.NoProgress, title, output)
    var currentProgress: Long = 0
    val advance = (inc: Long, total: Long) => {
      console.setProgress(
        ProgressUpdate.RemainingElements(currentProgress, total)
      )
      currentProgress += inc
    }
    advance(0, 0)
    timed(
      output.stream,
      title,
      console(_ => op(advance))
    )
  }

  private def timed[T](
      stream: PrintStream,
      title: String,
      op: => Try[T]
  ): Try[T] = {
    val timer = new Timer(Time.system)

    def failed(): Unit = {
      val msg = fansi.Color.Red(title + " failed") ++ fansi.Str(s" in $timer")
      stream.println(msg.render)
    }

    try {
      val result = op
      result match {
        case Failure(_) =>
          failed()
        case _ =>
          val msg =
            fansi.Color.Green(title) ++ fansi.Str(s" finished in $timer")
          stream.println(msg.render)
      }
      result
    } catch {
      case NonFatal(ex) =>
        failed()
        throw ex
    }
  }
}
