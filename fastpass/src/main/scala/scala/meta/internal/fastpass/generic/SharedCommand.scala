package scala.meta.internal.fastpass.generic

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.sys.process._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import scala.meta.internal.fastpass.BuildInfo
import scala.meta.internal.fastpass.FastpassEnrichments._
import scala.meta.internal.fastpass.LogMessages
import scala.meta.internal.fastpass.MessageOnlyException
import scala.meta.internal.fastpass.Timer
import scala.meta.internal.fastpass.pantsbuild.PantsExportResult

import metaconfig.cli.CliApp
import metaconfig.cli.TabCompletionContext
import metaconfig.cli.TabCompletionItem
import metaconfig.internal.Levenshtein

object SharedCommand {

  def postExportActions(
      app: CliApp,
      project: Project,
      export: ExportOptions,
      open: OpenOptions,
      importMode: ImportMode,
      timer: Timer,
      installResult: Try[Option[PantsExportResult]]
  ): Int = {
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
          project.common,
          export.coursierBinary,
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
          project.common,
          app,
          isStrict = false
        )

        if (export.canBloopExit) {
          SharedCommand.restartBloopIfNewSettings(app, exportResult)
        }
        if (open.isEmpty) {
          OpenCommand.onEmpty(project, app)
        } else {
          OpenCommand.run(
            open
              .withProject(project)
              .withWorkspace(project.common.workspace),
            app
          )
        }
        0
    }
  }

  def restartBloopIfNewSettings(
      app: CliApp,
      exportResult: Option[PantsExportResult]
  ): Unit = {
    val javaHome =
      exportResult.flatMap(
        _.javaHome
      ) //exportResult.flatMap(_.pantsExport.jvmDistribution.javaHome)
    val usedJavaHome = if (javaHome.isDefined) javaHome else defaultJavaHomePath
    val isUpdatedBloopSettings =
      BloopGlobalSettings.update(usedJavaHome)
    if (isUpdatedBloopSettings) {
      restartBloopServer()
    } else {
      restartOldBloopServer(app)
    }
  }

  private def deleteSymlinkDirectories(project: Project): Unit = {
    project.bspRoot.list
      .map(_.toNIO)
      .filter(path =>
        Files.isSymbolicLink(path) &&
          Files.isDirectory(Files.readSymbolicLink(path))
      )
      .foreach { symlink => Files.deleteIfExists(symlink) }
  }

  def withOneProject(
      action: String,
      projects: List[String],
      common: SharedOptions,
      app: CliApp
  )(fn: Project => Int): Int =
    projects match {
      case Nil =>
        app.error(s"no projects to $action")
        1
      case name :: Nil =>
        Project.fromName(name, common) match {
          case Some(project) =>
            fn(project)
          case None =>
            SharedCommand.noSuchProject(name, app, common)
        }
      case projects =>
        app.error(
          s"expected 1 project to $action but received ${projects.length} arguments '${projects.mkString(" ")}'"
        )
        1
    }

  def noSuchProject(name: String, app: CliApp, common: SharedOptions): Int = {
    val candidates = Project.names(common)
    val closest = Levenshtein.closestCandidate(name, candidates)
    val didYouMean = closest match {
      case Some(candidate) => s"\n\tDid you mean '$candidate'?"
      case None => ""
    }
    app.error(s"project '$name' does not exist$didYouMean")
    1
  }

  def complete(
      context: TabCompletionContext,
      allowsMultipleProjects: Boolean = false
  ): List[TabCompletionItem] = {
    Project
      .fromCommon(SharedOptions())
      .map(project => TabCompletionItem(project.name))
  }

  /** Upgrades the Bloop server if it's known to be an old version. */
  private def restartOldBloopServer(app: CliApp): Unit = {
    val isOutdated = Set[String](
      "1.4.0-RC1-235-3231567a", "1.4.0-RC1-190-ef7d8dba",
      "1.4.0-RC1-167-61fbbe08", "1.4.0-RC1-69-693de22a",
      "1.4.0-RC1+33-dfd03f53", "1.4.0-RC1", "1.4.1", "1.4.2", "1.4.3", "1.4.4",
      "1.4.6", "1.4.8"
    )
    Try {
      val version = List("bloop", "about").!!.linesIterator
        .find(_.startsWith("bloop v"))
        .getOrElse("")
        .stripPrefix("bloop v")

      if (isOutdated(version)) {
        scribe.info(s"shutting down old version of Bloop '$version'")
        restartBloopServer()
      }
    }.recover {
      case e: Throwable => app.error(e.getLocalizedMessage)
    }
  }

  private def restartBloopServer(): Unit = {
    List("bloop", "exit").!
    List(
      "bloop",
      "about",
      s"--fallback-bloop-version=${BuildInfo.bloopVersion}"
    ).!
  }

  private def defaultJavaHomePath: Option[Path] = {
    defaultJavaHome.map(Paths.get(_))
  }

  private def defaultJavaHome: Option[String] = {
    Option(System.getenv("JAVA_HOME")).orElse(
      Option(System.getProperty("java.home"))
    )
  }

  def runScalafmtSymlink(
      project: Project,
      common: SharedOptions
  ): Unit = {
    val workspace = common.workspace
    val out = project.root.bspRoot.toNIO
    val inScalafmt = {
      var link = workspace.resolve(".scalafmt.conf")
      // Configuration file may be symbolic link.
      while (Files.isSymbolicLink(link)) {
        link = Files.readSymbolicLink(link)
      }
      // Symbolic link may be relative to workspace directory.
      if (link.isAbsolute()) link
      else workspace.resolve(link)
    }
    val outScalafmt = out.resolve(".scalafmt.conf")
    if (
      !out.startsWith(workspace) &&
      Files.exists(inScalafmt) && {
        !Files.exists(outScalafmt) ||
        Files.isSymbolicLink(outScalafmt)
      }
    ) {
      Files.deleteIfExists(outScalafmt)
      Files.createDirectories(outScalafmt.getParent())
      Files.createSymbolicLink(outScalafmt, inScalafmt)
    }
  }

}
