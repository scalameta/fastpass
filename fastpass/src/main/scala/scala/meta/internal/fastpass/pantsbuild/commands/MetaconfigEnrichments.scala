package scala.meta.internal.fastpass.pantsbuild.commands

import fansi.Color
import fansi.Str
import metaconfig.cli.CliApp

object MetaconfigEnrichments {
  implicit class XtensionCliApp(app: CliApp) {
    def warn(message: Str): Unit = {
      app.out.println(Color.LightYellow("warn: ") ++ message)
    }
    def info(message: Str): Unit = {
      app.out.println(Color.LightBlue("info: ") ++ message)
    }
  }
}
