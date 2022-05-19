package scala.meta.internal.fastpass.console

trait Progress {
  def consume(b: Int): Option[ProgressUpdate]
}

object Progress {
  object NoProgress extends Progress {
    override def consume(b: Int): Option[ProgressUpdate] = None
  }
}
