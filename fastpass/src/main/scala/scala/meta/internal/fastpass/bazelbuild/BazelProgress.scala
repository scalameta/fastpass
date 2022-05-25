package scala.meta.internal.fastpass.bazelbuild

import java.text.DecimalFormatSymbols

import scala.meta.internal.fastpass.bazelbuild.BazelProgress._
import scala.meta.internal.fastpass.console.Progress
import scala.meta.internal.fastpass.console.ProgressUpdate

class BazelProgress extends Progress {

  private[this] var currentState: State = Empty
  override def consume(b: Int): Option[ProgressUpdate.RemainingElements] = {
    { currentState = currentState.consume(b); currentState } match {
      case FinalState(finished, total) =>
        Some(ProgressUpdate.RemainingElements(finished, total))
      case _ =>
        None
    }
  }
}

private object BazelProgress {
  sealed trait State {
    def consume(b: Int): State
  }
  case object Empty extends State {
    override def consume(b: Int): State =
      if (b == '[') RBracket
      else Empty
  }

  case object RBracket extends State {
    override def consume(b: Int): State =
      if (isNumber(b)) FinishedNumber(1, b - '0')
      else Empty
  }

  case class FinishedNumber(digits: Int, buffer: Long) extends State {
    override def consume(b: Int): State =
      if (isNumber(b)) FinishedNumber(digits + 1, buffer * 10 + (b - '0'))
      else if (isSeparator(b)) this
      else if (b == ' ') FirstSpace(buffer)
      else Empty
  }

  case class FirstSpace(finishedNumber: Long) extends State {
    override def consume(b: Int): State =
      if (b == '/') Slash(finishedNumber)
      else Empty
  }

  case class Slash(finishedNumber: Long) extends State {
    override def consume(b: Int): State =
      if (b == ' ') SecondSpace(finishedNumber)
      else Empty
  }

  case class SecondSpace(finishedNumber: Long) extends State {
    override def consume(b: Int): State =
      if (isNumber(b)) TotalNumber(finishedNumber, 1, b - '0')
      else Empty
  }

  case class TotalNumber(finishedNumber: Long, digits: Int, buffer: Long)
      extends State {
    override def consume(b: Int): State =
      if (isNumber(b))
        TotalNumber(finishedNumber, digits + 1, buffer * 10 + (b - '0'))
      else if (isSeparator(b)) this
      else if (b == ']') FinalState(finishedNumber, buffer)
      else Empty
  }

  case class FinalState(finishedNumber: Long, totalNumber: Long) extends State {
    override def consume(b: Int): State = Empty.consume(b)
  }

  private val separator: Int =
    DecimalFormatSymbols.getInstance().getGroupingSeparator()

  private def isNumber(b: Int): Boolean =
    b >= '0' && b <= '9'

  private def isSeparator(b: Int): Boolean =
    b == separator

}
