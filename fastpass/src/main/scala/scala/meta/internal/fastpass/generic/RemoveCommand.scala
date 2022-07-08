package scala.meta.internal.fastpass.generic

import scala.meta.internal.fastpass.RecursivelyDelete

import metaconfig.cli.CliApp
import metaconfig.cli.Command
import metaconfig.cli.Messages
import metaconfig.cli.TabCompletionContext
import metaconfig.cli.TabCompletionItem
import org.typelevel.paiges.Doc

object RemoveCommand extends Command[RemoveOptions]("remove") {
  override def description: Doc = Doc.paragraph("Delete existing projects")
  override def options: Doc = Messages.options(RemoveOptions())
  override def examples: Doc =
    Doc.text("fastpass remove PROJECT_NAME1 PROJECT_NAME2")
  override def complete(
      context: TabCompletionContext
  ): List[TabCompletionItem] =
    SharedCommand.complete(context, allowsMultipleProjects = true)
  def run(remove: RemoveOptions, app: CliApp): Int = {
    SharedCommand.withAtLeastOneProject(
      "remove",
      remove.projects,
      remove.common,
      app
    ) { project =>
      app.info(s"removing directory '${project.root.bspRoot}'")
      RecursivelyDelete(project.root.bspRoot)
      0
    }
  }
}
