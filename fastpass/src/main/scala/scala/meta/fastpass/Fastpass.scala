package scala.meta.fastpass

import scala.meta.internal.fastpass.BuildInfo
import scala.meta.internal.fastpass.FastpassLogger
import scala.meta.internal.fastpass.SystemProcess
import scala.meta.internal.fastpass.generic.AmendCommand
import scala.meta.internal.fastpass.generic.CreateCommand
import scala.meta.internal.fastpass.generic.CurrentCommand
import scala.meta.internal.fastpass.generic.InfoCommand
import scala.meta.internal.fastpass.generic.ListCommand
import scala.meta.internal.fastpass.generic.OpenCommand
import scala.meta.internal.fastpass.generic.RefreshCommand
import scala.meta.internal.fastpass.generic.RemoveCommand
import scala.meta.internal.fastpass.generic.SwitchCommand

import metaconfig.cli.CliApp
import metaconfig.cli.HelpCommand
import metaconfig.cli.TabCompleteCommand
import metaconfig.cli.VersionCommand

class Fastpass
object Fastpass {

  lazy val app: CliApp = CliApp(
    version = BuildInfo.fastpassVersion,
    binaryName = "fastpass",
    commands = List(
      HelpCommand,
      VersionCommand,
      CurrentCommand,
      CreateCommand,
      RefreshCommand,
      ListCommand,
      InfoCommand,
      OpenCommand,
      SwitchCommand,
      AmendCommand,
      RemoveCommand,
      TabCompleteCommand
    )
  )

  def main(args: Array[String]): Unit = {
    FastpassLogger.updateDefaultFormat()
    val exit = app.run(args.toList)
    System.exit(exit)
  }

  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run(): Unit = {
      SystemProcess.RunningProcesses.destroyAll()
    }
  })

}
