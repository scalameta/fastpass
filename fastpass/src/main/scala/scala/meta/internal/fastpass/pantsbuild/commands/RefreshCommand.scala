package scala.meta.internal.fastpass.pantsbuild.commands

import scala.meta.internal.fastpass.pantsbuild.Export

import metaconfig.cli.CliApp
import metaconfig.cli.Command
import metaconfig.cli.Messages
import metaconfig.cli.TabCompletionContext
import metaconfig.cli.TabCompletionItem
import org.typelevel.paiges.Doc

object RefreshCommand extends Command[RefreshOptions]("refresh") {
  override def description: Doc = Doc.paragraph("Refresh an existing project")
  override def options: Doc = Messages.options(RefreshOptions())
  override def examples: Doc =
    Doc.intercalate(
      Doc.line,
      List(
        "# Refresh a project and launch IntelliJ after the refresh completes",
        "fastpass refresh --intellij PROJECT_NAME"
      ).map(Doc.text)
    )
  override def complete(
      context: TabCompletionContext
  ): List[TabCompletionItem] =
    SharedCommand.complete(context, allowsMultipleProjects = true)
  def run(refresh: RefreshOptions, app: CliApp): Int = {
    val projects = Project.fromCommon(refresh.common)
    val errors = refresh.projects.map { projectName =>
      projects.find(_.matchesName(projectName)) match {
        case Some(project) =>
          SharedCommand.interpretExport(
            Export(
              project.copy(
                sources = refresh.export.sources
                  .toNonDefaultWithFallback(project.sources),
                strictDeps = refresh.export.strictDeps
                  .toNonDefaultWithFallback(project.strictDeps)
              ),
              refresh.open,
              app
            ).copy(
              export = refresh.export.copy(
                sources = refresh.export.sources,
                strictDeps = refresh.export.strictDeps
              ),
              isCache = refresh.update
            )
          )
        case None =>
          SharedCommand.noSuchProject(projectName, app, refresh.common)
      }
    }
    errors.sum
  }
}
