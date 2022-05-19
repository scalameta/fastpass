package scala.meta.internal.fastpass.generic

import scala.util.Failure
import scala.util.Try

import scala.meta.internal.fastpass.MessageOnlyException
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
      existingProjects.find(_.matchesName(projectName)) match {
        case None =>
          SharedCommand.noSuchProject(projectName, app, refresh.common)
        case Some(project) if project.importMode == ImportMode.Pants =>
          reportFailure(app, PantsRefresh.run(refresh, project, app))
        case Some(project) if project.importMode == ImportMode.Bazel =>
          reportFailure(
            app,
            BloopBazel.run(
              project,
              refresh.common.workspace,
              refresh.common.bazelBinary,
              refresh.open.intellij,
              app
            )
          )
      }
    }.sum
  }

  private def reportFailure[T](app: CliApp, op: Try[T]): Int =
    op match {
      case Failure(MessageOnlyException(msg)) =>
        app.error(msg)
        1
      case Failure(other) =>
        app.error(s"fastpass failed to run")
        other.printStackTrace(app.out)
        1
      case _ =>
        0
    }
}
