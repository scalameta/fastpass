package scala.meta.internal.fastpass.generic

import java.io.File
import java.nio.file.Files

import scala.meta.internal.fastpass.FastpassEnrichments._
import scala.meta.internal.fastpass.Time
import scala.meta.internal.fastpass.Timer
import scala.meta.internal.fastpass.bazelbuild.Bazel
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
    Doc.paragraph("Create a new project from a list of targets")
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
        "# Create project with an auto-generated name, infer Bazel",
        "fastpass create --name PROJECT_NAME TARGETS1/... TARGETS2/...", "",
        "# Create project with an auto-generated name and launch IntelliJ",
        "fastpass create --intellij TARGETS/..."
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
  def run(rawOptions: CreateOptions, app: CliApp): Int = {
    val create = inferOptions(rawOptions)
    val name = create.actualName
    Project.fromName(name, create.common) match {
      case Some(value) =>
        if (value.matchesCreateOptions(create)) {
          app.info(s"Project '$name' already exists; refreshing.")
          val refreshOptions = RefreshOptions(
            projects = name :: Nil,
            stopAfterCache = create.stopAfterCache,
            export = create.export,
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
                create.stopAfterCache,
                app
              )
          }
        SharedCommand.postExportActions(
          app,
          project,
          create.export,
          create.open,
          importMode,
          timer,
          installResult
        )
    }
  }

  private def inferOptions(create: CreateOptions): CreateOptions = {
    def isDir(target: String): Boolean =
      Files.isDirectory(
        create.common.workspace.resolve(target.stripPrefix("//"))
      )

    // If we find a recursive wildcard import, then we know it's a bazel import
    val isBazel = create.bazel || create.targets.exists(tgt =>
      tgt.endsWith("/...") || tgt.startsWith("//")
    )

    if (isBazel) {
      val targets = create.targets.map { target =>
        // Don't touch targets that look like Bazel queries
        if (!Bazel.isPlainSpec(target)) {
          target
        }
        // Translate foo/bar to foo/bar/... This is inconsistent with Pants (foo/bar:bar),
        // but more likely to be what the user meant: import directory recursively.
        else if (
          !target.contains(":") && !target.endsWith("/...") && isDir(target)
        ) {
          target + "/..."
        }
        // Translate foo/bar:: to foo/bar/...
        else if (target.endsWith("::") && isDir(target.stripSuffix("::"))) {
          target.stripSuffix("::") + "/..."
        }
        // Translate foo/bar: to foo/bar:all
        else if (target.endsWith(":") && isDir(target.stripSuffix(":"))) {
          target + "all"
        } else target
      }

      create.copy(bazel = true, targets = targets)
    } else {
      create
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
