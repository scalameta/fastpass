package scala.meta.internal.fastpass.generic

import scala.meta.internal.fastpass.Time
import scala.meta.internal.fastpass.Timer
import scala.meta.internal.fastpass.bazelbuild.BloopBazel
import scala.meta.internal.fastpass.pantsbuild.commands.PantsRefresh

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
    val existingProjects = Project.fromCommon(refresh.common)
    refresh.projects.map { projectName =>
      val timer = new Timer(Time.system)
      existingProjects.find(_.matchesName(projectName)) match {
        case None =>
          SharedCommand.noSuchProject(projectName, app, refresh.common)
        case Some(project) if project.importMode == ImportMode.Pants =>
          val installResult = PantsRefresh.run(refresh, project, app)
          SharedCommand.postExportActions(
            app,
            project,
            refresh.export,
            refresh.open,
            ImportMode.Pants,
            timer,
            installResult
          )
        case Some(project) if project.importMode == ImportMode.Bazel =>
          val installResult = BloopBazel.run(
            project,
            refresh.common.workspace,
            refresh.common.bazelBinary,
            refresh.open.intellij,
            app
          )
          SharedCommand.postExportActions(
            app,
            project,
            refresh.export,
            refresh.open,
            ImportMode.Bazel,
            timer,
            installResult
          )
      }
    }.sum
  }
}
