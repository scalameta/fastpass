package scala.meta.internal.fastpass

import scribe._
import scribe.format._

object FastpassLogger {

  def updateDefaultFormat(): Unit = {
    Logger.root
      .clearHandlers()
      .withHandler(
        formatter = defaultFormat,
        minimumLevel = Some(scribe.Level.Info),
        modifiers = Nil
      )
      .replace()
  }

  def defaultFormat: Formatter = formatter"$date $levelPaddedRight $message"

}
