package scala.meta.internal.fastpass.bazelbuild

import java.io.File
import java.io.OutputStream
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable
import scala.util.Success
import scala.util.Try

import scala.meta.internal.fastpass.FileUtils
import scala.meta.internal.fastpass.bazelbuild.AnalysisProtosV2.Artifact
import scala.meta.internal.fastpass.bazelbuild.Build.Attribute
import scala.meta.internal.fastpass.bazelbuild.Build.Target
import scala.meta.internal.fastpass.console.ProgressConsole
import scala.meta.internal.fastpass.generic.DependencyResolution
import scala.meta.internal.fastpass.generic.Project
import scala.meta.internal.fastpass.pantsbuild.PantsExportResult
import scala.meta.internal.fastpass.pantsbuild.PantsGlobs
import scala.meta.io.AbsolutePath

import bloop.config.Config
import bloop.config.Tag
import metaconfig.cli.CliApp

object BloopBazel {
  def run(
      project: Project,
      workspace: Path,
      bazelBinary: Path,
      intellij: Boolean,
      stopAfterCache: Boolean,
      app: CliApp
  ): Try[Option[PantsExportResult]] = {
    if (intellij) Success(None)
    else {
      val bazel = new Bazel(bazelBinary, workspace)
      for {
        bazelInfo <- bazel.info()
        scalaJars <- DependencyResolution.scalaJars
        testFrameworksJars <- DependencyResolution.testingFrameworkJars
        bloopBazel <- getBloopBazel(
          app,
          project,
          bazel,
          bazelInfo,
          scalaJars,
          testFrameworksJars
        )
      } yield {
        if (stopAfterCache) Success(None)
        else bloopBazel.run()
      }
    }.flatten
  }

  private def getBloopBazel(
      app: CliApp,
      project: Project,
      bazel: Bazel,
      bazelInfo: BazelInfo,
      scalaJars: List[Path],
      testFrameworksJars: List[Path]
  ): Try[BloopBazel] = {
    val cache = RemoteCache.configure(bazel, project)

    cache
      .getFromCache(cachedExportName) { export =>
        app.info("Using cached Fastpass export.")
        BloopBazel.fromExport(
          project,
          bazel,
          bazelInfo,
          scalaJars,
          testFrameworksJars,
          export
        )
      }
      .recoverWith {
        case err =>
          app.warn(s"No cached export available: ${err.getMessage}")
          val importableRules = supportedRules.keys.toList
          for {
            dependencies <- bazel.dependenciesToBuild(
              project.targets,
              importableRules,
              forbiddenGenerators
            )
            importedTargets <- bazel.targetsInfos(
              project.targets,
              importableRules,
              forbiddenGenerators
            )
            actions <- bazel.aquery(project.targets)
            actionGraph = ActionGraph(actions)
            targetGlobs <- targetToSources(importedTargets, bazel)
            rawInputs = rawTargetInputs(importedTargets, actionGraph)
            rawRuntimeInputs =
              rawRuntimeTargetInputs(importedTargets, actionGraph)
          } yield {
            val bloopBazel = new BloopBazel(
              project,
              bazel,
              bazelInfo,
              dependencies,
              importedTargets,
              actionGraph,
              targetGlobs,
              rawInputs,
              rawRuntimeInputs,
              scalaJars,
              testFrameworksJars
            )
            cache
              .writeToCache(cachedExportName)(bloopBazel.writeExport)
              .fold(
                ex => app.warn(s"Write to cached failed: ${ex.getMessage}"),
                hasWritten =>
                  if (hasWritten) app.info("Export written to cache.")
                  else ()
              )
            bloopBazel
          }
      }
  }

  private def fromExport(
      project: Project,
      bazel: Bazel,
      bazelInfo: BazelInfo,
      scalaJars: List[Path],
      testFrameworksJars: List[Path],
      export: java.io.InputStream
  ): BloopBazel = {
    val js = ujson.read(export)
    val protoIndex = js("protoIndex").arr.value
    val dependenciesToBuild = js("dependenciesToBuild").arr.toList.map(_.str)
    val importedTargets = js("importedTargets").arr.toList
      .map(JsonUtils.jsonToProto(protoIndex, _)(Target.parseFrom))
    val actionGraph = ActionGraph.fromJson(protoIndex, js("actionGraph"))
    val targetGlobs =
      JsonUtils.mapFromJson(
        js("targetGlobs"),
        "target",
        JsonUtils.jsonToProto(protoIndex, _)(Target.parseFrom),
        "globs",
        js => PantsGlobs.fromJson(ujson.Obj("globs" -> js))
      )
    val rawTargetInputs =
      JsonUtils.mapFromJson(
        js("rawTargetInputs"),
        "target",
        JsonUtils.jsonToProto(protoIndex, _)(Target.parseFrom),
        "artifacts",
        _.arr.toList
          .map(JsonUtils.jsonToProto(protoIndex, _)(Artifact.parseFrom))
      )
    val rawRuntimeTargetInputs =
      JsonUtils.mapFromJson(
        js("rawRuntimeTargetInputs"),
        "target",
        JsonUtils.jsonToProto(protoIndex, _)(Target.parseFrom),
        "artifacts",
        _.arr.toList
          .map(JsonUtils.jsonToProto(protoIndex, _)(Artifact.parseFrom))
      )

    new BloopBazel(
      project,
      bazel,
      bazelInfo,
      dependenciesToBuild,
      importedTargets,
      actionGraph,
      targetGlobs,
      rawTargetInputs,
      rawRuntimeTargetInputs,
      scalaJars,
      testFrameworksJars
    )
  }

  private def copyImmutableJars(
      bazelInfo: BazelInfo,
      dest: Path,
      allInputs: Set[Artifact],
      actionGraph: ActionGraph
  ): Try[CopiedJars] = {
    def withSuffix(path: Path, strip: String, add: String): Path = {
      path.resolveSibling(
        path.getFileName().toString().stripSuffix(strip) + add
      )
    }
    val external = bazelInfo.bazelBin.resolve("external").toNIO
    val sourceJars = mutable.Buffer.empty[Path]
    val inputMapping = mutable.Map.empty[Artifact, Path]
    // Ignore artifacts whose path end with these suffixes:
    //  - sources will be provided by Bloop
    val ignoredArtifactSuffixes = List(".scala", ".java")
    ProgressConsole
      .foreach("Copying JAR dependencies", allInputs) { input =>
        val artifactPath = actionGraph.pathOf(input)

        if (ignoredArtifactSuffixes.exists(artifactPath.endsWith)) {
          ()
        } else if (artifactPath.startsWith("external/")) {
          val mappedPath = bazelInfo.outputBase.resolve(artifactPath).toNIO
          val possibleSourceJarPath =
            withSuffix(mappedPath, ".jar", "-sources.jar")
          if (Files.exists(possibleSourceJarPath)) {
            sourceJars += possibleSourceJarPath
          }

          inputMapping += input -> mappedPath
        } else {
          // The `Scalac` action of Java targets uses interface JARs as input.
          // We swap those for the actual JARs in Bloop, because we will need those
          // to run the tests.
          val selectedArtifact =
            maybeExcludeIJar(bazelInfo.executionRoot.toNIO, artifactPath)
          val fullPath = bazelInfo.executionRoot.toNIO.resolve(selectedArtifact)

          if (Files.exists(fullPath) && !fullPath.startsWith(external)) {
            val possibleSourceJarsPaths = List(
              withSuffix(fullPath, ".jar", "-src.jar"),
              withSuffix(fullPath, ".jar", "_java-src.jar")
            )
            possibleSourceJarsPaths.filter(Files.exists(_)).foreach {
              sourceJar =>
                val mappedPath = dest.resolve(sourceJar.getFileName.toString)
                Files.createDirectories(mappedPath.getParent())
                Files.copy(
                  sourceJar,
                  mappedPath,
                  StandardCopyOption.REPLACE_EXISTING
                )
                sourceJars += mappedPath
            }

            val mappedPath = dest.resolve(selectedArtifact)
            Files.createDirectories(mappedPath.getParent())
            Files.copy(
              fullPath,
              mappedPath,
              StandardCopyOption.REPLACE_EXISTING
            )
            inputMapping += input -> mappedPath
          } else {
            val possibleSourceJarPath =
              withSuffix(fullPath, ".jar", "-sources.jar")
            if (Files.exists(possibleSourceJarPath)) {
              sourceJars += possibleSourceJarPath
            }
            inputMapping += input -> fullPath
          }
        }
      }
      .map { _ =>
        CopiedJars(inputMapping.toMap, sourceJars.toList)
      }
  }

  private def copyJars(
      bazelInfo: BazelInfo,
      project: Project,
      importedTargets: List[Target],
      actionGraph: ActionGraph,
      rawTargetInputs: Map[Target, List[Artifact]],
      rawRuntimeTargetInputs: Map[Target, List[Artifact]]
  ): Try[CopiedJars] = {
    val fromOutputs = importedTargets.flatMap { target =>
      val label = target.getRule().getName()
      actionGraph.outputsOf(label).map { output =>
        output -> targetDirectory(project, bloopName(target))
          .resolve("classes")
          .toNIO
      }
    }.toMap

    val allInputs =
      rawTargetInputs.valuesIterator.flatten.toSet ++ rawRuntimeTargetInputs.valuesIterator.flatten.toSet -- fromOutputs.keySet

    val fromInputs =
      copyImmutableJars(
        bazelInfo,
        project.bspRoot.toNIO,
        allInputs,
        actionGraph
      )

    fromInputs.map {
      case CopiedJars(inputMappings, sourceJars) =>
        CopiedJars(inputMappings ++ fromOutputs, sourceJars)
    }
  }

  private def targetDirectory(
      project: Project,
      bloopName: String
  ): AbsolutePath = {
    val dirName = FileUtils.sanitizeFileName(bloopName)
    val dir = project.bspRoot.resolve("out").resolve(dirName)
    Files.createDirectories(dir.toNIO)
    dir
  }

  private def bloopName(target: Target): String =
    target.getRule().getName().stripPrefix("//")

  private def rawTargetInputs(
      importedTargets: List[Target],
      actionGraph: ActionGraph
  ): Map[Target, List[Artifact]] = {
    importedTargets.map { target =>
      val ruleClass = target.getRule().getRuleClass()
      val mnemonics = supportedRules.getOrElse(ruleClass, Set.empty)
      val label = target.getRule.getName()
      target -> actionGraph.transitiveInputsOf(label, mnemonics)
    }.toMap
  }

  private def rawRuntimeTargetInputs(
      importedTargets: List[Target],
      actionGraph: ActionGraph
  ): Map[Target, List[Artifact]] = {
    importedTargets.flatMap { target =>
      val label = target.getRule().getName()
      val inputs = actionGraph.transitiveInputsOf(label, Set("Middleman"))
      if (inputs.nonEmpty) Some(target -> inputs)
      else None
    }.toMap
  }

  private def maybeExcludeIJar(base: Path, path: String): String = {
    if (path.endsWith("-ijar.jar")) {
      val notIJar = path.stripSuffix("-ijar.jar") + ".jar"
      val maybeNotIJar = base.resolve(notIJar)
      if (Files.exists(maybeNotIJar)) notIJar
      else path
    } else path
  }

  private def targetToSources(
      importedTargets: List[Target],
      bazel: Bazel
  ): Try[Map[Target, PantsGlobs]] = {
    val packages =
      importedTargets.map(_.getRule().getName().takeWhile(_ != ':')).distinct
    bazel.sourcesGlobs(packages).map { labelToSources =>
      val mappings = for {
        target <- importedTargets
        label = target.getRule().getName()
        (includes, excludes) <- labelToSources.get(label)
        globs = PantsGlobs(includes, excludes)
      } yield target -> globs
      mappings.toMap
    }
  }

  private val supportedRules: Map[String, Set[String]] = Map(
    "scala_library" -> Set("Scalac"),
    "_java_library" -> Set("Scalac"),
    "_scala_macro_library" -> Set("Scalac"),
    "scala_junit_test" -> Set("Scalac")
  )

  private val forbiddenGenerators: List[String] = List(
    "create_datasets",
    "antlr"
  )

  private val cachedExportName: String = "fastpass-export.json"

  /**
   * Returns the toplevel directories that enclose all of the target.
   *
   * For example, this method returns the directories `a/src` and `b` given the
   * targets below:
   *
   * - a/src:foo
   * - a/src/inner:bar
   * - b:b
   * - b/inner:c
   */
  private def sourceRoots(
      workspace: AbsolutePath,
      bazelSpecs: List[String],
      bazelTargets: List[String]
  ): List[AbsolutePath] = {
    // We inspect both the specs that were specified by the user and the targets
    // that ended up being imported.
    // If we user imported using bazel queries, these sets may be quite different.
    val fromSpecs = bazelSpecs.flatMap {
      case s if s.contains("(") => None
      case s if s.endsWith("/...") =>
        Some(s.stripPrefix("//").stripSuffix("/..."))
      case s => Some(s.replaceFirst("/?:.*", "").stripPrefix("//"))
    }
    val fromTargets =
      bazelTargets.map(_.replaceFirst("/?:.*", "").stripPrefix("//"))
    val parts = (fromSpecs ++ fromTargets).sorted
    if (parts.isEmpty) Nil
    else {
      val buf = mutable.ListBuffer.empty[String]
      var current = parts(0)
      buf += current
      parts.iterator.drop(1).foreach { target =>
        if (!target.startsWith(current)) {
          current = target
          buf += current
        }
      }
      buf.result().map(workspace.resolve)
    }
  }
}

private class BloopBazel(
    project: Project,
    bazel: Bazel,
    bazelInfo: BazelInfo,
    dependenciesToBuild: List[String],
    importedTargets: List[Target],
    actionGraph: ActionGraph,
    targetGlobs: Map[Target, PantsGlobs],
    rawTargetInputs: Map[Target, List[Artifact]],
    rawRuntimeTargetInputs: Map[Target, List[Artifact]],
    scalaJars: List[Path],
    testFrameworksJars: List[Path]
) {

  def run(): Try[Option[PantsExportResult]] = {
    val bloopDir = this.project.bspRoot.resolve(".bloop").toNIO
    Files.createDirectories(bloopDir)
    for {
      _ <- bazel.build(dependenciesToBuild, "_source_jars" :: Nil)
      inputsMapping <- BloopBazel.copyJars(
        bazelInfo,
        project,
        importedTargets,
        actionGraph,
        rawTargetInputs,
        rawRuntimeTargetInputs
      )
      projects <- assembleBloopProjects(inputsMapping)
      syntheticProjects = assembleSyntheticProjects(projects)
      _ <- writeBloopConfigFiles(bloopDir, projects ++ syntheticProjects)
    } yield Some(
      new PantsExportResult(
        projects.length + syntheticProjects.length,
        Map.empty,
        None,
        Iterator.empty
      )
    )
  }

  private def assembleSyntheticProjects(
      projects: List[Config.Project]
  ): List[Config.Project] = {
    val sourceRoots = BloopBazel.sourceRoots(
      bazelInfo.workspace,
      project.targets,
      importedTargets.map(_.getRule.getName)
    )
    val isBaseDirectory =
      projects.filter(_.sourcesGlobs.nonEmpty).map(_.directory).toSet
    sourceRoots.flatMap { root =>
      if (isBaseDirectory(root.toNIO)) Nil
      else {
        val name = root
          .toRelative(bazelInfo.workspace)
          .toURI(isDirectory = false)
          .toString()
        List(emptyBloopProject(name + "-project-root", root.toNIO))
      }
    }
  }

  private def assembleBloopProjects(
      inputsMapping: CopiedJars
  ): Try[List[Config.Project]] = {
    ProgressConsole.map(
      "Assembling Bloop configurations",
      importedTargets
    )(bloopProject(inputsMapping, _))
  }

  private def writeBloopConfigFiles(
      bloopDir: Path,
      projects: List[Config.Project]
  ): Try[Unit] = {
    ProgressConsole.foreach("Writing Bloop configuration files", projects) {
      project =>
        val file = Config.File("1.5.0", project)
        val out = bloopDir.resolve(FileUtils.makeJsonFilename(project.name))
        bloop.config.write(file, out)
    }
  }

  private def writeExport(out: OutputStream): Unit =
    ProgressConsole
      .auto("Caching export") { _ =>
        val newJson = ujson.Obj()
        val protoIndex = new JsonUtils.ProtoIndex
        newJson("importedTargets") =
          importedTargets.map(JsonUtils.protoToJson(protoIndex, _))
        newJson("dependenciesToBuild") = dependenciesToBuild
        newJson("actionGraph") = actionGraph.toJson(protoIndex)
        newJson("targetGlobs") = JsonUtils.mapToJson(targetGlobs)(
          "target",
          JsonUtils.protoToJson(protoIndex, _),
          "globs",
          _.toJson
        )
        newJson("rawTargetInputs") = JsonUtils.mapToJson(rawTargetInputs)(
          "target",
          JsonUtils.protoToJson(protoIndex, _),
          "artifacts",
          _.map(JsonUtils.protoToJson(protoIndex, _))
        )
        newJson("rawRuntimeTargetInputs") =
          JsonUtils.mapToJson(rawRuntimeTargetInputs)(
            "target",
            JsonUtils.protoToJson(protoIndex, _),
            "artifacts",
            _.map(JsonUtils.protoToJson(protoIndex, _))
          )
        newJson("protoIndex") = protoIndex.toJson
        newJson.writeBytesTo(out)
      }
      .get

  private def dependencies(target: Target): List[Target] =
    targetDependencies.get(target).getOrElse(Nil)

  private def classpath(
      inputsMapping: CopiedJars,
      target: Target
  ): List[Path] = {
    val inputs = rawTargetInputs.getOrElse(target, Nil)
    inputs
      .flatMap(inputsMapping.artifactToPath.get)
      .distinct
      .filterNot(_.getFileName().toString.endsWith("-mval.jar"))
  }

  private def runtimeClasspath(
      inputsMapping: CopiedJars,
      target: Target
  ): List[Path] = {
    val inputs = rawRuntimeTargetInputs.getOrElse(
      target,
      rawTargetInputs.getOrElse(target, Nil)
    )
    val entries = inputs.flatMap(inputsMapping.artifactToPath.get).distinct
    if (isTest(target)) entries ++ testFrameworksJars
    else entries
  }

  private def javaOptions(target: Target): List[String] = {
    getAttribute(target, "jvm_flags") match {
      case Some(attr) =>
        val flags = attr.getStringListValueList().asScala.toList
        s"-Duser.dir=${bazelInfo.workspace}" :: flags
      case None =>
        Nil
    }
  }

  private def bloopProject(
      inputsMapping: CopiedJars,
      target: Target
  ): Config.Project = {
    val projectName = BloopBazel.bloopName(target)
    val projectDirectory =
      project.common.workspace.resolve(projectName.takeWhile(_ != ':'))
    val (sources, sourcesGlobs) = targetSourcesAndGlobs(target)
    val deps = dependencies(target).map(BloopBazel.bloopName)
    val targetDir =
      BloopBazel.targetDirectory(project, BloopBazel.bloopName(target))
    val resources =
      if (isResources(target)) {
        Some(List(projectDirectory))
      } else {
        None
      }
    val jvmConfig = Config.JvmConfig(
      home = Some(bazelInfo.javaHome.toNIO),
      options = javaOptions(target)
    )
    val jvmPlatform = Config.Platform.Jvm(
      jvmConfig,
      mainClass = None,
      runtimeConfig = None,
      classpath = Some(runtimeClasspath(inputsMapping, target)),
      resources = None
    )

    Config.Project(
      name = projectName,
      directory = projectDirectory,
      workspaceDir = Some(project.common.workspace),
      sources = sources.map(project.common.workspace.resolve(_)),
      sourcesGlobs = sourcesGlobs.map(globs =>
        List(globs.bloopConfig(project.common.workspace, projectDirectory))
      ),
      sourceRoots = Some(List(projectDirectory)),
      dependencies = deps,
      classpath = classpath(inputsMapping, target),
      out = targetDir.resolve("out").toNIO,
      classesDir = targetDir.resolve("classes").toNIO,
      resources = resources,
      `scala` = scala(target),
      java = Some(java(target)),
      sbt = None,
      test = Some(test(target)),
      platform = Some(jvmPlatform),
      resolution = Some(resolution(inputsMapping.sourceJars)),
      tags = Some(List(if (isTest(target)) Tag.Test else Tag.Library))
    )
  }

  // Returns a Bloop project that has no source code. This project only exists
  // to control for example how the project view is displayed in IntelliJ.
  private def emptyBloopProject(
      name: String,
      directory: Path
  ): Config.Project = {
    val directoryName = FileUtils.makeClassesDirFilename(name)
    val targetDir = BloopBazel.targetDirectory(project, name)
    Config.Project(
      name = name,
      directory = directory,
      workspaceDir = Some(bazelInfo.workspace.toNIO),
      sources = Nil,
      sourcesGlobs = None,
      sourceRoots = None,
      dependencies = Nil,
      classpath = Nil,
      out = targetDir.resolve("out").toNIO,
      classesDir = targetDir.resolve("classes").toNIO,
      // NOTE(olafur): we generate a fake resource directory so that IntelliJ
      // displays this directory in the "Project files tree" view. This needs to
      // be a resource directory instead of a source directory to prevent Bloop
      // from compiling it.
      resources = Some(List(directory)),
      scala = None,
      java = None,
      sbt = None,
      test = None,
      platform = None,
      resolution = None,
      tags = None
    )
  }

  private def resolution(sourceJars: List[Path]): Config.Resolution = {
    Config.Resolution(
      sourceJars.map { jar =>
        Config.Module(
          organization = "",
          name = "",
          version = "",
          configurations = None,
          artifacts = List(
            Config.Artifact(
              name = "",
              classifier = Some("sources"),
              checksum = None,
              path = jar
            )
          )
        )
      }
    )
  }

  private def test(target: Target) =
    Config.Test(
      frameworks = List(
        Config.TestFramework(names =
          List("munit.internal.junitinterface.PantsFramework")
        )
      ),
      options = Config.TestOptions.empty
    )

  private def java(target: Target) =
    Config.Java(
      options = Nil
    )

  private def scala(target: Target): Option[Config.Scala] = {
    val options = getAttribute(target, "scalacopts").map(
      _.getStringListValueList().asScala.toList
    )

    for {
      opts <- options
    } yield Config.Scala(
      organization = "org.scala-lang",
      name = "scala-compiler",
      version = "2.12.15",
      options = opts,
      jars = scalaJars,
      analysis = None,
      setup = Some(
        Config.CompileSetup(
          order = Config.Mixed,
          addLibraryToBootClasspath = true,
          addCompilerToClasspath = false,
          addExtraJarsToClasspath = false,
          manageBootClasspath = true,
          filterLibraryFromClasspath = true
        )
      )
    )
  }

  private val rawOutputToTarget: Map[Artifact, Target] = {
    val mappings =
      for {
        target <- importedTargets
        label = target.getRule().getName()
        artifact <- actionGraph.outputsOf(label)
      } yield artifact -> target
    mappings.toMap
  }

  private val targetDependencies: Map[Target, List[Target]] = {
    importedTargets.map { target =>
      rawTargetInputs.get(target) match {
        case None => target -> Nil
        case Some(inputs) =>
          target -> inputs
            .flatMap(rawOutputToTarget.get(_))
            .filterNot(_ == target)
            .distinct
      }
    }.toMap
  }

  private def targetSourcesAndGlobs(
      target: Target
  ): (List[String], Option[PantsGlobs]) =
    if (isResources(target)) (Nil, None)
    else
      targetGlobs.get(target) match {
        case Some(globs) if globs.isEmpty =>
          // Finding empty globs means that buildozer was able to extract the target from
          // the build file, but no sources were set. This means we need to use the default
          // globs.
          (Nil, Some(defaultGlobs(target)))
        case Some(globs) =>
          (Nil, Some(globs))
        case None =>
          // If the target doesn't appear in `targetGlobs`, it means that buildozer was not
          // able to read it from the build file. It's probably generated by a macro, so we
          // hardcode only the sources that Bazel knows about.
          val sources = getAttribute(target, "srcs")
            .map(_.getStringListValueList().asScala.toList.map {
              _.replaceAllLiterally(":", File.separator).stripPrefix("//")
            })
            .getOrElse(Nil)
          (sources, None)
      }

  private def isTest(target: Target): Boolean = {
    target.getRule().getRuleClass() == "scala_junit_test"
  }

  private def defaultGlobs(target: Target): PantsGlobs = {
    generatorFunction(target) match {
      case Some("scala_library") => PantsGlobs("*.scala" :: Nil, Nil)
      case Some("java_library") => PantsGlobs("*.java" :: Nil, Nil)
      case Some("junit_tests") => PantsGlobs("*.scala" :: "*.java" :: Nil, Nil)
      case _ => PantsGlobs.empty
    }
  }

  private def generatorFunction(target: Target): Option[String] = {
    getAttribute(target, "generator_function").map(_.getStringValue())
  }

  private def isResources(target: Target): Boolean = {
    target.getRule().getRuleClass() == "scala_library" && generatorFunction(
      target
    ).exists(_ == "resources")
  }

  private def getAttribute(
      target: Target,
      attribute: String
  ): Option[Attribute] =
    target.getRule().getAttributeList().asScala.find(_.getName() == attribute)
}

private case class CopiedJars(
    artifactToPath: Map[Artifact, Path],
    sourceJars: List[Path]
)
