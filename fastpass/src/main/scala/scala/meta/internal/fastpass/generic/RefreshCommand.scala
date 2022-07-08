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
    SharedCommand.withAtLeastOneProject(
      "refresh",
      refresh.projects,
      refresh.common,
      app
    ) {
      case project if project.importMode == ImportMode.Pants =>
        val timer = new Timer(Time.system)
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
      case project if project.importMode == ImportMode.Bazel =>
        val timer = new Timer(Time.system)
        val installResult = BloopBazel.run(
          project,
          refresh.common.workspace,
          refresh.common.bazelBinary,
          refresh.open.intellij,
          refresh.ignoreCache,
          refresh.stopAfterCache,
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
  }
}
