package scala.meta.internal.fastpass.console

trait ProgressUpdate {
  def render(width: Int): String
}

object ProgressUpdate {
  case class RemainingElements(
      finished: Long,
      total: Long
  ) extends ProgressUpdate {
    val progress: Double = if (total == 0L) 0 else finished.toDouble / total
    override def render(width: Int): String = {
      val numberEnd = width / 2 + 1
      val progressPercent = Math.round(progress * 100).toInt
      val progressPerWidth = Math.floor(progress * width).toInt
      val text = String.format(
        s"%${numberEnd}d%%${" " * (width - numberEnd - 1)}",
        progressPercent: Integer
      )
      val formatted =
        fansi.Reversed.On(text.substring(0, progressPerWidth)) ++ fansi.Str(
          text.substring(progressPerWidth)
        )
      formatted.render
    }
  }

  class Spinner() extends ProgressUpdate {
    private[this] var state = 0.0
    override def render(width: Int): String = {
      val progress = Math.abs(Math.sin(state))
      val progressPerWidth = Math.floor(progress * width).toInt
      val formatted = fansi.Reversed.On(" " * progressPerWidth) ++ fansi.Str(
        " " * (width - progressPerWidth)
      )
      state += Math.PI / 100
      formatted.render
    }
  }
}
