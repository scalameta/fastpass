package scala.meta.internal.fastpass.bazelbuild

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
      app: CliApp
  ): Try[Option[PantsExportResult]] = {
    if (intellij) Success(None)
    else {
      val bazel = new Bazel(bazelBinary, workspace)
      val importableRules = supportedRules.keys.toList
      for {
        bazelInfo <- bazel.info()
        dependencies <- bazel.dependenciesToBuild(
          project.targets,
          importableRules,
          forbiddenGenerators
        )
        _ <- bazel.build(dependencies, "_source_jars" :: Nil)
        importedTargets <- bazel.targetsInfos(
          project.targets,
          importableRules,
          forbiddenGenerators
        )
        actions <- bazel.aquery(project.targets)
        actionGraph = ActionGraph(actions)
        rawInputs = rawTargetInputs(importedTargets, actionGraph)
        rawRuntimeInputs = rawRuntimeTargetInputs(importedTargets, actionGraph)
        inputMappings <- copyJars(
          bazelInfo,
          project,
          importedTargets,
          actionGraph,
          rawInputs,
          rawRuntimeInputs
        )
        scalaJars <- DependencyResolution.scalaJars
        testFrameworksJars <- DependencyResolution.testingFrameworkJars
        bloopBazel = new BloopBazel(
          project,
          bazel,
          bazelInfo,
          importedTargets,
          actionGraph,
          inputMappings,
          rawInputs,
          rawRuntimeInputs,
          scalaJars,
          testFrameworksJars
        )
      } yield bloopBazel.run()
    }
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
        output -> targetDirectory(project, target).resolve("classes").toNIO
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
      target: Target
  ): AbsolutePath = {
    val dirName = FileUtils.sanitizeFileName(bloopName(target))
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

}

private class BloopBazel(
    project: Project,
    bazel: Bazel,
    bazelInfo: BazelInfo,
    importedTargets: List[Target],
    actionGraph: ActionGraph,
    inputsMapping: CopiedJars,
    rawTargetInputs: Map[Target, List[Artifact]],
    rawRuntimeTargetInputs: Map[Target, List[Artifact]],
    scalaJars: List[Path],
    testFrameworksJars: List[Path]
) {

  def run(): Option[PantsExportResult] = {
    val bloopDir = this.project.bspRoot.resolve(".bloop").toNIO
    Files.createDirectories(bloopDir)

    ProgressConsole.foreach(
      "Generating Bloop configuration files",
      importedTargets
    ) { target =>
      val project = bloopProject(target)
      val file = Config.File("1.5.0", project)
      val out = bloopDir.resolve(FileUtils.makeJsonFilename(project.name))
      bloop.config.write(file, out)
    }

    Some(
      new PantsExportResult(
        importedTargets.length,
        Map.empty,
        None,
        Iterator.empty
      )
    )
  }

  def dependencies(target: Target): List[Target] =
    targetDependencies.get(target).getOrElse(Nil)

  def classpath(target: Target): List[Path] = {
    val inputs = rawTargetInputs.getOrElse(target, Nil)
    inputs
      .flatMap(inputsMapping.artifactToPath.get)
      .distinct
      .filterNot(_.getFileName().toString.endsWith("-mval.jar"))
  }

  def runtimeClasspath(target: Target): List[Path] = {
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

  private def bloopProject(target: Target): Config.Project = {
    val projectName = BloopBazel.bloopName(target)
    val projectDirectory =
      project.common.workspace.resolve(projectName.takeWhile(_ != ':'))
    val sourcesGlobs = targetSources(target)
    val deps = dependencies(target).map(BloopBazel.bloopName)
    val targetDir = BloopBazel.targetDirectory(project, target)
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
      classpath = Some(runtimeClasspath(target)),
      resources = None
    )

    Config.Project(
      name = projectName,
      directory = projectDirectory,
      workspaceDir = Some(project.common.workspace),
      sources = Nil,
      sourcesGlobs =
        if (resources.isDefined) None
        else
          Some(
            List(
              sourcesGlobs
                .bloopConfig(project.common.workspace, projectDirectory)
            )
          ),
      sourceRoots = Some(List(projectDirectory)),
      dependencies = deps,
      classpath = classpath(target),
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

  private val targetsSources = targetToSources()
  private def targetSources(target: Target): PantsGlobs =
    if (isResources(target)) PantsGlobs.empty
    else targetsSources.getOrElse(target, defaultGlobs(target))

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

  private def targetToSources(): Map[Target, PantsGlobs] = {
    val packages =
      importedTargets.map(_.getRule().getName().takeWhile(_ != ':')).distinct
    val labelToSources = bazel.sourcesGlobs(packages)
    importedTargets.flatMap { target =>
      labelToSources.get(target.getRule().getName()).map {
        case (includes, excludes) =>
          val globs = PantsGlobs(includes, excludes)
          target -> globs
      }
    }.toMap
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
