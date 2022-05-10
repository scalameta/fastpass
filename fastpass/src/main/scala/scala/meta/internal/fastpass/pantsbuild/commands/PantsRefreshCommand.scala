package scala.meta.internal.fastpass.pantsbuild.commands

import scala.util.Try

import scala.meta.internal.fastpass.generic.Project
import scala.meta.internal.fastpass.generic.RefreshOptions
import scala.meta.internal.fastpass.generic.SharedCommand
import scala.meta.internal.fastpass.pantsbuild.Export
import scala.meta.internal.fastpass.pantsbuild.PantsExportResult

import metaconfig.cli.CliApp
import metaconfig.cli.Command
import metaconfig.cli.Messages
import metaconfig.cli.TabCompletionContext
import metaconfig.cli.TabCompletionItem
import org.typelevel.paiges.Doc

object PantsRefresh {
  def run(
      options: RefreshOptions,
      project: Project,
      app: CliApp
  ): Try[Option[PantsExportResult]] = {
    SharedPantsCommand.interpretExport(
      Export(
        project.copy(
          sources =
            options.export.sources.toNonDefaultWithFallback(project.sources),
          strictDeps = options.export.strictDeps
            .toNonDefaultWithFallback(project.strictDeps)
        ),
        options.open,
        app
      ).copy(
        export = options.export.copy(
          sources = options.export.sources,
          strictDeps = options.export.strictDeps
        ),
        isCache = options.update
      )
    )
  }
}

object PantsRefreshCommand extends Command[RefreshOptions]("refresh") {
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
          SharedPantsCommand
            .interpretExport(
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
            .fold(_ => 0, _ => 1)
        case None =>
          SharedCommand.noSuchProject(projectName, app, refresh.common)
      }
    }
    errors.sum
  }
}
