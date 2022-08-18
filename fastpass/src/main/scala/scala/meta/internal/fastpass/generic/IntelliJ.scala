package scala.meta.internal.fastpass.generic

import java.net.URL
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption

import scala.sys.process._

import scala.meta.fastpass.Fastpass
import scala.meta.internal.fastpass.FastpassEnrichments._
import scala.meta.internal.fastpass.pantsbuild.PantsExportResult
import scala.meta.internal.fastpass.zipkin.Property
import scala.meta.internal.fastpass.zipkin.ZipkinProperties
import scala.meta.internal.fastpass.{BuildInfo => V}

import bloop.data.TraceSettings
import bloop.data.WorkspaceSettings
import bloop.io.AbsolutePath
import bloop.logging.NoopLogger
import ujson.Obj
import ujson.Str

object IntelliJ {
  def launch(project: Project, open: OpenOptions): Unit = {
    val applications = Paths.get("/Applications")
    val candidates = List(
      applications.resolve("Twitter IntelliJ IDEA.app"),
      applications.resolve("Twitter IntelliJ IDEA CE.app"),
      applications.resolve("IntelliJ IDEA.app"),
      applications.resolve("IntelliJ IDEA CE.app")
    )
    def macosOpen(app: String): List[String] =
      List("open", "-a", app)
    val command = open.intellijLauncher match {
      case Some(launcher) =>
        if (launcher.endsWith(".app")) macosOpen(launcher)
        else List(launcher)
      case None =>
        candidates.find(Files.isDirectory(_)) match {
          case Some(intellij) =>
            macosOpen(intellij.toString())
          case None =>
            List("idea")
        }
    }
    val openDirectory = project.bspRoot
    val exit = Process(
      command ++ List(openDirectory.toString),
      cwd = Some(openDirectory.toFile)
    ).!
    if (exit != 0) {
      scribe.error(s"failed to launch IntelliJ: '${command.mkString(" ")}'")
    }
  }

  /** The .bsp/bloop.json file is necessary for IntelliJ to automatically import the project */
  def writeBsp(
      project: Project,
      shared: SharedOptions,
      coursierBinary: Option[Path] = None,
      exportResult: Option[PantsExportResult] = None
  ): Unit = {
    SharedCommand.runScalafmtSymlink(project, shared)
    val bspJson = project.root.bspJson.toNIO
    Files.createDirectories(bspJson.getParent)
    val coursier = coursierBinary.getOrElse(
      downloadCoursier(bspJson.resolveSibling("coursier"))
    )
    val newJson = Obj()
    newJson("name") = "Bloop"
    newJson("version") = V.bloopNightlyVersion
    newJson("bspVersion") = V.bspVersion
    newJson("languages") = List[String]("scala", "java")
    newJson("argv") = List[String](
      coursier.toString,
      "launch",
      s"ch.epfl.scala:bloop-launcher-core_2.12:${V.bloopNightlyVersion}",
      "--ttl",
      "Inf",
      "--",
      V.bloopVersion
    )
    newJson("argvExit") = List[String](
      coursier.toString,
      "launch",
      s"bloop:${V.bloopVersion}",
      "--",
      "exit"
    )
    newJson("sources") = project.sources.toNonDefault.toString
    newJson("strictDeps") = project.strictDeps.toNonDefault.toString
    newJson("importMode") = project.importMode.name
    newJson("pantsTargets") = project.targets
    newJson("fastpassVersion") = V.fastpassVersion
    newJson("fastpassProjectName") = project.name
    newJson("pantsTargets") = project.targets
    newJson("X-detectExternalProjectFiles") = false
    Files.write(
      bspJson,
      newJson.render(indent = 2).getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.CREATE
    )

    val refreshCommand = List(
      coursier.toString,
      "launch",
      s"org.scalameta:fastpass_2.12:${V.fastpassVersion}",
      "-r",
      "sonatype:snapshots",
      "--ttl",
      "Inf",
      "--main",
      classOf[Fastpass].getName,
      "--",
      RefreshCommand.name,
      "--workspace",
      project.common.workspace.toString,
      "--no-bloop-exit"
    ) ++
      shared.pants
        .map(path => List("--pants", path.toString))
        .getOrElse(List.empty) :+
      project.name

    val workspace = scala.meta.io.AbsolutePath(project.common.workspace)
    val props = Property.fromFile(workspace)

    val traceSettings = TraceSettings(
      ZipkinProperties.zipkinServerUrl.value(props),
      Property.booleanValue(ZipkinProperties.debugTracing, props),
      Property.booleanValue(ZipkinProperties.verbose, props),
      ZipkinProperties.localServiceName.value(props),
      ZipkinProperties.traceStartAnnotation.value(props),
      ZipkinProperties.traceEndAnnotation.value(props)
    )

    val configDir = AbsolutePath(project.root.bloopRoot.toNIO)
    if (!configDir.exists) configDir.createDirectories
    val currentSettings = WorkspaceSettings
      .readFromFile(configDir, NoopLogger)
      .getOrElse(WorkspaceSettings(None, None, None, None, None))
    val settings =
      currentSettings.copy(
        refreshProjectsCommand = Some(refreshCommand),
        traceSettings = Some(traceSettings)
      )
    WorkspaceSettings.writeToFile(configDir, settings, NoopLogger)
    exportResult.foreach(r => writeLibraryDependencies(project, r))
  }

  private def downloadCoursier(destination: Path): Path = {
    if (Files.isRegularFile(destination) && Files.isExecutable(destination)) {
      destination
    } else if (Files.exists(destination)) {
      throw new IllegalArgumentException(s"file already exists: destination")
    } else {
      val url = sys.env
        .getOrElse("FASTPASS_COURSIER_URL", "https://git.io/coursier-cli")
      Files.copy(
        new URL(url).openConnection().getInputStream(),
        destination
      )
      destination.toFile.setExecutable(true)
      destination
    }
  }

  private def writeLibraryDependencies(
      project: Project,
      export: PantsExportResult
  ): Unit = {
    val libraries = Obj()
    export.libraries.foreach { obj =>
      for {
        default <- obj.default
        sources <- obj.sources
      } {
        libraries(default.toString()) = Str(sources.toString())
      }
    }
    export.internalSources.foreach {
      case (default, sources) =>
        libraries(default.toString()) = Str(sources.toString())
    }
    project.root.pantsLibrariesJson.writeText(ujson.write(libraries, indent = 4))
  }
}
