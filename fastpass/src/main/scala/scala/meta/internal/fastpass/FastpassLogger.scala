package scala.meta.internal.fastpass

import java.io.PrintStream
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import scala.meta.io.AbsolutePath
import scala.meta.io.RelativePath
import scribe._
import scribe.format._
import scribe.modify.LogModifier
import scribe.writer.FileWriter

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
