package scala.meta.internal.fastpass.generic

import metaconfig.cli.CliApp
import metaconfig.cli.Command
import org.typelevel.paiges.Doc

object CurrentCommand extends Command[CurrentOptions]("current") {
  override def description: Doc =
    Doc.paragraph("Show the name of the current project")
  def run(options: CurrentOptions, app: CliApp): Int = {
    Project.current(options.common) match {
      case None =>
        app.error(
          "No active Fastpass project in the current directory. To fix this problem, run `fastpass list` to see available projects and `fastpass switch PROJECT_NAME` to activate a project."
        )
        1
      case Some(project) =>
        println(project.name)
        0
    }
  }
}
