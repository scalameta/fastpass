package scala.meta.internal.fastpass.generic

import java.io.File

import scala.util.Failure
import scala.util.Success

import scala.meta.internal.fastpass.FastpassEnrichments._
import scala.meta.internal.fastpass.LogMessages
import scala.meta.internal.fastpass.MessageOnlyException
import scala.meta.internal.fastpass.Time
import scala.meta.internal.fastpass.Timer
import scala.meta.internal.fastpass.bazelbuild.BloopBazel
import scala.meta.internal.fastpass.pantsbuild.Export
import scala.meta.internal.fastpass.pantsbuild.commands.SharedPantsCommand
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath

import metaconfig.cli.CliApp
import metaconfig.cli.Command
import metaconfig.cli.Messages
import metaconfig.cli.TabCompletionContext
import metaconfig.cli.TabCompletionItem
import org.typelevel.paiges.Doc

object CreateCommand extends Command[CreateOptions]("create") {
  override def description: Doc =
    Doc.paragraph("Create a new project from a list of Pants targets")
  override def usage: Doc =
    Doc.text("fastpass create [OPTIONS] [TARGETS ...]")
  override def options: Doc = Messages.options(CreateOptions())
  override def examples: Doc =
    Doc.intercalate(
      Doc.line,
      List(
        "# Create project with custom name from two Pants targets",
        "fastpass create --name PROJECT_NAME TARGETS1:: TARGETS2::", "",
        "# Create project with an auto-generated name with Bazel",
        "fastpass create --bazel --name PROJECT_NAME TARGETS1/... TARGETS2/...",
        "", "# Create project with an auto-generated name and launch IntelliJ",
        "fastpass create --intellij TARGETS::"
      ).map(Doc.text)
    )
  override def complete(
      context: TabCompletionContext
  ): List[TabCompletionItem] = {
    val isBazel = context.arguments.contains("--bazel")
    context.setting match {
      case None =>
        completeTargetSpec(context, PathIO.workingDirectory, isBazel)
      case _ =>
        Nil
    }
  }
  def run(create: CreateOptions, app: CliApp): Int = {
    val name = create.actualName
    Project.fromName(name, create.common) match {
      case Some(value) =>
        if (value.matchesCreateOptions(create)) {
          app.info(s"Project '$name' already exists; refreshing.")
          val refreshOptions = RefreshOptions(
            projects = name :: Nil,
            open = create.open,
            common = create.common
          )
          RefreshCommand.run(refreshOptions, app)
        } else {
          app.error(
            s"can't create project named '${name}' because it already exists, and was created with a different setup." +
              s"\n\tTo refresh the project run: 'fastpass refresh ${name}'" +
              s"\n\tTo open the project run: 'fastpass open --intellij ${name}'" +
              s"\n\t                     or: 'fastpass open --vscode ${name}'"
          )
          if (create.open.launch(value))
            0
          else
            1
        }

      case None =>
        val importMode =
          if (create.bazel) ImportMode.Bazel else ImportMode.Pants
        val project = Project.create(
          name,
          create.common,
          create.targets,
          sources = create.export.sources.toNonDefault,
          strictDeps = create.export.strictDeps.toNonDefault,
          importMode = importMode
        )
        val timer = new Timer(Time.system)
        val installResult =
          importMode match {
            case ImportMode.Pants =>
              SharedPantsCommand.interpretExport(
                Export(project, create.open, app).copy(export = create.export)
              )
            case ImportMode.Bazel =>
              BloopBazel.run(
                project,
                create.common.workspace,
                create.common.bazelBinary,
                create.open.intellij,
                app
              )
          }
        installResult match {
          case Failure(exception) =>
            exception match {
              case MessageOnlyException(message) =>
                app.error(message)
              case _ =>
                app.error(s"fastpass failed to run")
                exception.printStackTrace(app.out)
            }
            1
          case Success(exportResult) =>
            IntelliJ.writeBsp(
              project,
              create.common,
              create.export.coursierBinary,
              exportResult
            )
            exportResult.foreach { result =>
              val targets =
                LogMessages.pluralName(
                  s"${importMode} target",
                  result.exportedTargets
                )
              app.info(
                s"exported ${targets} to project '${project.name}' in $timer"
              )
            }
            SwitchCommand.runSymlinkOrWarn(
              project,
              create.common,
              app,
              isStrict = false
            )

            if (create.export.canBloopExit) {
              SharedCommand.restartBloopIfNewSettings(app, exportResult)
            }
            if (create.open.isEmpty) {
              OpenCommand.onEmpty(project, app)
            } else {
              OpenCommand.run(
                create.open
                  .withProject(project)
                  .withWorkspace(create.common.workspace),
                app
              )
            }
            0
        }
    }
  }

  private def completeTargetSpec(
      context: TabCompletionContext,
      cwd: AbsolutePath,
      isBazel: Boolean
  ): List[TabCompletionItem] = {
    val suffix = if (isBazel) "/..." else "::"
    val path =
      context.last.split(File.separatorChar).foldLeft(cwd) {
        case (dir, "") => dir
        case (dir, name) => dir.resolve(name)
      }
    val toList =
      if (
        context.last.isEmpty ||
        context.last.endsWith(File.separator)
      ) path
      else path.parent
    toList.list
      .filter(_.isDirectory)
      .filter(!_.filename.startsWith("."))
      .map(name =>
        name
          .toRelative(cwd)
          .toURI(isDirectory = false)
          .toString() + suffix
      )
      .map(name => TabCompletionItem(name))
      .toBuffer
      .toList
  }
}
