package scala.meta.internal.fastpass.bazelbuild

import java.io.File
import java.io.OutputStream
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.nio.file.attribute.PosixFilePermission
import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.tailrec
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.setAsJavaSetConverter
import scala.collection.mutable
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal

import scala.meta.internal.fastpass.FileUtils
import scala.meta.internal.fastpass.MessageOnlyException
import scala.meta.internal.fastpass.bazelbuild.AnalysisProtosV2.Artifact
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

  final val CacheFormatVersion = "3"

  def run(
      project: Project,
      workspace: Path,
      bazelBinary: Path,
      intellij: Boolean,
      ignoreCache: Boolean,
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
          ignoreCache,
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
      ignoreCache: Boolean,
      project: Project,
      bazel: Bazel,
      bazelInfo: BazelInfo,
      scalaJars: List[Path],
      testFrameworksJars: List[Path]
  ): Try[BloopBazel] = {
    val cache =
      if (ignoreCache) {
        new NoRemoteCache(
          new MessageOnlyException("Cache ignored with `--ignore-cache`")
        )
      } else {
        RemoteCache.configure(bazel, project, CacheFormatVersion)
      }

    cache
      .getFromCache(cachedExportName) { export =>
        app.info("Using cached Fastpass export.")
        BloopBazel.fromExport(
          app,
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
            rawDependencies <- bazel.dependenciesToBuild(
              project.targets,
              importableRules,
              forbiddenGenerators,
              forbiddenTags
            )
            // Always build the Scrooge worker, since we may need it in case we import Thrift
            // targets. It builds quickly, so it's easier to always build it rather than figuring
            // out whether we'll actually need it.
            dependencies = Bazel.ScroogeWorkerLabel :: rawDependencies
            importedTargets <- bazel.targetsInfos(
              project.targets,
              importableRules,
              forbiddenGenerators,
              forbiddenTags
            )
            extractors = new Extractors(importedTargets)
            // Additional labels for which to query the action graph:
            //  - the Scrooge worker, so that we know where to find the script to run to regenerate
            //    the sources.
            //  - the platformclasspath, so that we know which artifacts it produces, and we can
            //    exclude them from the compilation classpath. Bloop will provide the ones from the
            //    environment. Keeping that from Bazel will break go to definition on JDK classes.
            //  - the protobuf_java ijar, so that we can swap it for the actual full jar. Having the
            //    ijar on the compilation classpath breaks zinc, which will crash while analyzing
            //    classes that inherit from classes provided by the ijar.
            actionGraphLabels =
              Bazel.ScroogeWorkerLabel ::
                Bazel.PlatformClasspathLabel ::
                Bazel.ProtobufJava ::
                importedTargets.map(_.getRule.getName)
            actions <- bazel.aquery(actionGraphLabels)
            actionGraph = ActionGraph(actions)
            sourcesInfo <- targetToSources(extractors, importedTargets, bazel)
            rawInputs =
              rawTargetInputs(extractors, importedTargets, actionGraph)
            rawRuntimeInputs =
              rawRuntimeTargetInputs(importedTargets, actionGraph)
          } yield {
            val bloopBazel = new BloopBazel(
              app,
              project,
              bazel,
              bazelInfo,
              dependencies,
              importedTargets,
              extractors,
              actionGraph,
              sourcesInfo,
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
      app: CliApp,
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
    val sourcesInfo =
      JsonUtils.mapFromJson(
        js("sourcesInfo"),
        "target",
        JsonUtils.jsonToProto(protoIndex, _)(Target.parseFrom),
        "infos",
        js => SourcesInfo.fromJson(js)
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
      app,
      project,
      bazel,
      bazelInfo,
      dependenciesToBuild,
      importedTargets,
      new Extractors(importedTargets),
      actionGraph,
      sourcesInfo,
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
    val bazelBin = bazelInfo.bazelBin.toNIO
    val execRoot = bazelInfo.executionRoot.toNIO
    val external = bazelBin.resolve("external")
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
            maybeExcludeIJar(execRoot, artifactPath)
          val fullPath = rebaseBinDir(
            bazelInfo.executionRoot.toNIO,
            bazelBin,
            selectedArtifact
          )

          if (Files.exists(fullPath) && !fullPath.startsWith(external)) {
            val possibleSourceJarsPaths = List(
              withSuffix(fullPath, ".jar", "-src.jar"),
              withSuffix(fullPath, ".jar", "_java-src.jar"),
              withSuffix(fullPath, ".jar", ".srcjar")
            )
            possibleSourceJarsPaths.filter(Files.exists(_)).foreach {
              sourceJar =>
                val jarPath = execRoot.relativize(sourceJar)
                // IntelliJ will not consider `.srcjar`s as source jars. We change the suffix to `-sources.jar`.
                val mappedPath =
                  if (jarPath.getFileName.toString.endsWith(".srcjar"))
                    withSuffix(dest.resolve(jarPath), ".srcjar", "-sources.jar")
                  else dest.resolve(jarPath)
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
        val mappings =
          substituteProtobufJavaIjar(inputMapping.toMap, bazelInfo, actionGraph)
        CopiedJars(mappings, sourceJars.toList)
      }
  }

  /**
   * Substitute the Protobuf Java ijar for the full jar, if they exist.
   *
   * Having the ijar on the compilation classpath causes issues with Zinc during analysis.
   *
   * @param mappings The current mappings between artifact and path on disk
   * @param bazelInfo The path information from Bazel
   * @param actionGraph The action graph
   * @return The new mappings, where the ijar artifact is mapped to the full jar path.
   */
  private def substituteProtobufJavaIjar(
      mappings: Map[Artifact, Path],
      bazelInfo: BazelInfo,
      actionGraph: ActionGraph
  ): Map[Artifact, Path] = {
    val protobufJavaIJar =
      actionGraph
        .outputsOfMnemonic(Bazel.ProtobufJava, Bazel.ProtobufJavaMnemonic)
        .headOption
        .filter { artifact =>
          val path = actionGraph.pathOf(artifact)
          val fullPath = bazelInfo.executionRoot.toNIO.resolve(path)
          Files.exists(fullPath)
        }
    val protobufJavaFullJar =
      actionGraph
        .transitiveInputsOf(Bazel.ProtobufJava, Set(Bazel.ProtobufJavaMnemonic))
        .headOption
        .map(actionGraph.pathOf)
        .map(bazelInfo.executionRoot.toNIO.resolve)
        .filter(Files.exists(_))
    (protobufJavaIJar, protobufJavaFullJar) match {
      case (Some(iJar), Some(fullJar)) =>
        mappings + (iJar -> fullJar)
      case _ =>
        mappings
    }
  }

  private def generatedSourceDirectory(
      project: Project,
      bloopName: String
  ): AbsolutePath = {
    val dirName = FileUtils.sanitizeFileName(bloopName)
    val dir = project.bspRoot.resolve("source-generators").resolve(dirName)
    Files.createDirectories(dir.toNIO)
    dir
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
      extractors: Extractors,
      importedTargets: List[Target],
      actionGraph: ActionGraph
  ): Map[Target, List[Artifact]] = {
    import extractors._
    importedTargets.map { target =>
      val (label, mnemonics) = target match {
        case ThriftTarget(_, thriftRoot, mnemonic) =>
          (thriftRoot.getRule().getName(), Set(mnemonic))
        case other =>
          val rule = other.getRule().getRuleClass()
          val mnemonics = supportedRules.getOrElse(rule, Set.empty)
          (other.getRule().getName(), mnemonics)
      }
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

  private def rebaseBinDir(
      execRoot: Path,
      binDir: Path,
      pathStr: String
  ): Path = {
    val path = Paths.get(pathStr)
    val length = path.getNameCount()
    if (
      length >= 4 && path.getName(0).toString == "bazel-out" && path
        .getName(2)
        .toString == "bin"
    ) {
      binDir.resolve(path.subpath(3, length))
    } else {
      execRoot.resolve(path)
    }
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
      extractors: Extractors,
      importedTargets: List[Target],
      bazel: Bazel
  ): Try[Map[Target, SourcesInfo]] = {
    import extractors._

    @tailrec
    def labelFor(target: Target): String =
      target match {
        case ThriftRoot(buildName, _, _) =>
          buildName
        case ThriftTarget(_, root, _) =>
          labelFor(root)
        case _ =>
          target.getRule().getName
      }

    val packages = importedTargets.map(Bazel.enclosingPackage)
    bazel.sourcesGlobs(packages).map { labelToSources =>
      val mappings = for {
        target <- importedTargets
        label = labelFor(target)
        info <- labelToSources.get(label)
      } yield target -> info
      mappings.toMap
    }
  }

  private val supportedRules: Map[String, Set[String]] = Map(
    "alias" -> Set(),
    "target_union" -> Set(),
    "scala_library" -> Set("Scalac"),
    "_java_library" -> Set("Scalac", "Javac"),
    "_scala_macro_library" -> Set("Scalac"),
    "scala_junit_test" -> Set("Scalac", "Javac"),
    "scala_binary" -> Set("Middleman"),
    "_jvm_app" -> Set()
    // Uncomment once Thrift support is ready.
    // "scrooge_scala_library" -> Set("ScroogeRule", "Scalac"),
    // "scrooge_java_library" -> Set("ScroogeRule", "Javac"),
    // "thrift_library" -> Set("ScroogeRule")
  )

  private val forbiddenGenerators: Map[String, List[String]] = Map(
    "" -> List("create_datasets", "antlr"),
    "scala_library" -> List("jvm_app")
  )

  private val forbiddenTags: List[String] = List(
    "no-ide",
    "manual"
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
      case s if !Bazel.isPlainSpec(s) => None
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
    app: CliApp,
    project: Project,
    bazel: Bazel,
    bazelInfo: BazelInfo,
    dependenciesToBuild: List[String],
    importedTargets: List[Target],
    extractors: Extractors,
    actionGraph: ActionGraph,
    sourcesInfo: Map[Target, SourcesInfo],
    rawTargetInputs: Map[Target, List[Artifact]],
    rawRuntimeTargetInputs: Map[Target, List[Artifact]],
    scalaJars: List[Path],
    testFrameworksJars: List[Path]
) {
  import extractors._

  def run(): Try[Option[PantsExportResult]] = {
    val bloopDir = this.project.bspRoot.resolve(".bloop").toNIO
    Files.createDirectories(bloopDir)
    for {
      _ <- bazel.build(dependenciesToBuild, "_source_jars" :: Nil)
      inputsMapping <- copyJars(
        bazelInfo,
        project,
        importedTargets,
        actionGraph,
        rawTargetInputs,
        rawRuntimeTargetInputs
      )
      projects <- assembleBloopProjects(inputsMapping)
      bloopFiles <- writeBloopConfigFiles(bloopDir, projects)
      _ = FileUtils.cleanStaleBloopFiles(bloopDir, bloopFiles.toSet)
    } yield Some(
      new PantsExportResult(
        projects.length,
        Map.empty,
        None,
        Iterator.empty
      )
    )
  }

  // IntelliJ supports only a single target per directory. When targets share the same root
  // directory, we try to find a meaningful unique root directory for every target. If that's not
  // possible, we show a warning to the user.
  private def assignRootDirectory(
      targets: List[Target]
  ): List[(Path, Target)] = {
    val directoryToTargets = targets.groupBy { target =>
      val projectName = BloopBazel.bloopName(target)
      val projectPackage = projectName.takeWhile(_ != ':')
      project.common.workspace.resolve(projectPackage)
    }
    val (unique, duplicated) = directoryToTargets.partition(_._2.length == 1)

    val deduplicated = duplicated.foldLeft(unique) {
      case (acc, (directory, targets)) =>
        deduplicate(acc, directory, targets)
    }

    deduplicated.toList.flatMap {
      case (path, target :: Nil) =>
        (path, target) :: Nil
      case (path, targets) =>
        // Thrift targets always share the same root directory, and it's fine because they'll
        // all share the same "classpath"
        if (targets.exists(!isThriftTarget(_))) {
          app.warn(
            s"""The base directory '$path' is shared by the following targets:
               |${targets.map(_.getRule.getName).mkString(", ")}
               |
               |Some of these targets may not appear in IntelliJ and cause other issues. To fix
               |this problem, move these targets or their sources to a dedicated directory.
               |See http://go/111""".stripMargin
          )
        }
        targets.map((path, _))
    }
  }

  @annotation.tailrec
  private def deduplicate(
      acc: Map[Path, List[Target]],
      directory: Path,
      targets: List[Target]
  ): Map[Path, List[Target]] = {
    targets match {
      case Nil =>
        acc
      case target :: rest =>
        val currentSourceDirs = Bazel
          .getAttribute(target, "srcs")
          .map(_.getStringListValueList.asScala.toList)
          .getOrElse(Nil)
          .map { src =>
            val path = src.stripPrefix("//").replace(':', '/')
            path.take(path.lastIndexOf('/'))
          }
          .distinct

        // Look at the existing sources of the target. If they all live in the same directory,
        // try to use that directory as base directory for the target. Otherwise, record the conflict.
        val newMatch = currentSourceDirs match {
          case root :: Nil =>
            val rootPath = project.common.workspace.resolve(root)
            if (!acc.contains(rootPath))
              rootPath -> List(target)
            else
              directory -> (target :: acc.getOrElse(directory, Nil))

          case _ =>
            directory -> (target :: acc.getOrElse(directory, Nil))
        }

        deduplicate(acc + newMatch, directory, rest)
    }
  }

  private def assembleBloopProjects(
      inputsMapping: CopiedJars
  ): Try[List[Config.Project]] = {
    val targets =
      importedTargets.filterNot(_.getRule.getRuleClass == "thrift_library")

    val targetAndDirectory = assignRootDirectory(targets)
    ProgressConsole
      .map(
        "Assembling Bloop configurations",
        targetAndDirectory
      ) {
        case (directory, target) =>
          bloopProject(inputsMapping, target, directory)
      }
      .map { organicProjects =>
        val sourceRoots = BloopBazel.sourceRoots(
          bazelInfo.workspace,
          project.targets,
          targets.map(_.getRule.getName)
        )
        def isEmptyGlobs(globsOpt: Option[List[Config.SourcesGlobs]]): Boolean =
          globsOpt match {
            case None => true
            case Some(globs) => globs.forall(_.includes.isEmpty)
          }

        val sourceRootToProjects =
          organicProjects.groupBy(p => AbsolutePath(p.directory))

        // For source roots where no project live, create synthetic projects
        // that will act as source root so that targets under a same source
        // root are grouped together.
        val syntheticProjects =
          sourceRoots.filter(r => !sourceRootToProjects.contains(r)).map {
            root =>
              // NOTE(olafur): cannot be `name + "-root"` since that conflicts with the
              // IntelliJ-generated root project.
              val syntheticProjectName =
                root
                  .toRelative(bazelInfo.workspace)
                  .toURI(isDirectory = false)
                  .toString() + "-project-root"
              emptyBloopProject(syntheticProjectName, root.toNIO)
          }

        // For projects that live on source roots, if the project has only
        // empty source globs (eg. `jvm_bin` targets), add the "DS_Store
        // workaround", so that at least one source is matched and the project
        // acts as source root.
        val projectsWithDsStoreWorkaround = sourceRootToProjects.flatMap {
          case (root, projects @ fst :: rest)
              if sourceRoots.contains(root) && projects
                .forall(p => isEmptyGlobs(p.sourcesGlobs)) =>
            addDsStoreWorkaround(fst) :: rest
          case (_, projects) =>
            projects
        }

        syntheticProjects ++ projectsWithDsStoreWorkaround
      }
  }

  private def writeBloopConfigFiles(
      bloopDir: Path,
      projects: List[Config.Project]
  ): Try[List[Path]] = {
    ProgressConsole.map("Writing Bloop configuration files", projects) {
      project =>
        val file = Config.File("1.5.0", project)
        val out = bloopDir.resolve(FileUtils.makeJsonFilename(project.name))
        bloop.config.write(file, out)
        out
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
        newJson("sourcesInfo") = JsonUtils.mapToJson(sourcesInfo)(
          "target",
          JsonUtils.protoToJson(protoIndex, _),
          "infos",
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
    target match {
      // jvm_app targets depend only on the binary they wrap.
      case JvmAppTarget(_, bin) =>
        bin :: Nil
      case ThriftTarget(target, root, mnemonic) =>
        // A thrift target can only depend on other thrift targets. We add a dependency
        // on the implementation that matches the current mnemonic.
        targetDependencies
          .getOrElse(root, Nil)
          .collect {
            case ThriftRoot(_, java, scala) =>
              if (mnemonic == "Javac") java else scala
          }
          .flatten
      case _ =>
        targetDependencies.getOrElse(target, Nil).flatMap {
          // If a non-thrift target depends on a thrift root, add a dependency on all
          // thrift implementation targets.
          case ThriftRoot(_, java, scala) =>
            java ++ scala
          case other =>
            other :: Nil
        }
    }

  private def classpath(
      inputsMapping: CopiedJars,
      target: Target
  ): List[Path] =
    target match {
      // jvm_app targets have no sources to compile; their compile classpath can be empty.
      case JvmAppTarget(_, bin) =>
        Nil
      case _ =>
        val inputs = rawTargetInputs.getOrElse(target, Nil)
        inputs
          .flatMap(inputsMapping.artifactToPath.get)
          .distinct
          .filterNot(_.getFileName().toString.endsWith("-mval.jar"))
    }

  private def runtimeClasspath(
      inputsMapping: CopiedJars,
      target: Target
  ): List[Path] =
    target match {
      // jvm_app targets use the same runtime classpath as the scala_binary they wrap.
      case JvmAppTarget(_, bin) =>
        runtimeClasspath(inputsMapping, bin)
      case _ =>
        val inputs = rawRuntimeTargetInputs.getOrElse(
          target,
          rawTargetInputs.getOrElse(target, Nil)
        )
        val entries = inputs.flatMap(inputsMapping.artifactToPath.get).distinct
        if (isTest(target)) entries ++ testFrameworksJars
        else entries
    }

  private def javaOptions(target: Target): List[String] =
    target match {
      // jvm_app targets are run with the java options of the binary they wrap.
      case JvmAppTarget(_, bin) =>
        javaOptions(bin)
      case _ =>
        val options = Bazel
          .getAttribute(target, "jvm_flags")
          .map(_.getStringListValueList().asScala.toList)
          .getOrElse(Nil)
        s"-Duser.dir=${bazelInfo.workspace}" :: options
    }

  private def sourceGenerators(
      inputsMapping: CopiedJars,
      target: Target,
      projectDirectory: Path
  ): Option[List[Config.SourceGenerator]] = {
    target match {
      case ThriftTarget(_, root, mnemonic) =>
        val bloopName = BloopBazel.bloopName(target)
        val generatedSourcesDirectory =
          BloopBazel.generatedSourceDirectory(project, bloopName)

        // If this project depends on other Thrift targets, then we need to pass their Thrift
        // sources to the source generator of the current target.
        val thriftIncludes = {
          // The thrift includes provided by other targets in this import
          val internalThriftIncludes = dependencies(target).map { dep =>
            val depName = BloopBazel.bloopName(dep)
            val outDirectory =
              BloopBazel.generatedSourceDirectory(project, depName)
            outDirectory.resolve("thrift-sources.srcjar")
          }

          val externalThriftIncludes =
            actionGraph
              .transitiveInputsOf(root.getRule.getName, Set("ScroogeRule"))
              .filterNot(rawOutputToTarget.contains)
              .filter(artifact =>
                actionGraph.pathOf(artifact).endsWith("--thrift-root.jar")
              )
              .flatMap(inputsMapping.artifactToPath.get)
              .map(AbsolutePath(_))
              .distinct

          internalThriftIncludes ++ externalThriftIncludes
        }

        val globs = {
          val targetSources =
            targetSourcesAndGlobs(root, projectDirectory) match {
              case (_, None) => Nil
              case (_, Some(srcs)) =>
                srcs.bloopConfig(
                  project.common.workspace,
                  projectDirectory
                ) :: Nil
            }
          val includes = thriftIncludes.map { sourceJar =>
            val nioSourceJar = sourceJar.toNIO
            Config.SourcesGlobs(
              nioSourceJar.getParent,
              walkDepth = Some(1),
              includes = s"glob:${nioSourceJar.getFileName}" :: Nil,
              excludes = Nil
            )
          }

          targetSources ++ includes
        }

        val scroogeWorker = actionGraph
          .outputsOfMnemonic(
            Bazel.ScroogeWorkerLabel,
            Bazel.ScroogeWorkerMnemonic
          )
          .headOption
          .map { artifact =>
            val path = actionGraph.pathOf(artifact)
            bazelInfo.workspace.resolve(path)
          }

        scroogeWorker.map { worker =>
          val thriftScript = writeThriftSourceGeneratorScript(
            target,
            if (mnemonic == "Javac") "java" else "scala",
            generatedSourcesDirectory,
            thriftIncludes,
            worker
          )
          Config.SourceGenerator(
            sourcesGlobs = globs,
            outputDirectory =
              generatedSourcesDirectory.resolve("generated").toNIO,
            command = List(thriftScript.syntax)
          ) :: Nil
        }
      case _ =>
        None
    }
  }

  private def writeThriftSourceGeneratorScript(
      target: Target,
      language: String,
      generatedSourcesDirectory: AbsolutePath,
      thriftIncludes: List[AbsolutePath],
      scroogeWorker: AbsolutePath
  ): AbsolutePath = {
    val filename = FileUtils.sanitizeFileName(target.getRule.getName, ".sh")
    val scriptLocation = generatedSourcesDirectory.resolve(filename)
    val flags = Bazel
      .getAttribute(target, "compiler_args")
      .map(_.getStringListValueList().asScala)
      .getOrElse(Nil) ++ List("--language", language)
    val content =
      s"""#!/bin/bash -eu
         |OUTPUT_DIRECTORY="$$1"; shift
         |
         |BAZEL_WORKSPACE="${bazelInfo.workspace}"
         |SCROOGE_INPUT="$generatedSourcesDirectory/scrooge-input"
         |SCROOGE_SOURCES_JAR="$generatedSourcesDirectory/thrift-sources.srcjar"
         |SCROOGE_OUTPUT="$generatedSourcesDirectory/scrooge-output.jar"
         |echo "$$SCROOGE_OUTPUT" > "$$SCROOGE_INPUT"
         |echo "_$$SCROOGE_SOURCES_JAR" >> "$$SCROOGE_INPUT"
         |echo "_${thriftIncludes.mkString(":")}" >> "$$SCROOGE_INPUT"
         |echo "_" >> "$$SCROOGE_INPUT"
         |echo "_" >> "$$SCROOGE_INPUT"
         |echo "_${flags.mkString(":")}" >> "$$SCROOGE_INPUT"
         |
         |SHALLOWEST_SOURCE=""
         |MIN_DEPTH=1000
         |for ARG in "$$@"; do
         |  if [[ "$$ARG" == *.srcjar || "$$ARG" == *.jar ]]; then
         |    continue
         |  else
         |    CURRENT_DEPTH="$$(($$(echo "$$ARG" | tr -c -d '/' | wc -c) + 1))"
         |    if [ $$CURRENT_DEPTH -lt $$MIN_DEPTH ]; then
         |      SHALLOWEST_SOURCE="$$ARG"
         |      MIN_DEPTH=$$CURRENT_DEPTH
         |    fi
         |  fi
         |done
         |
         |if [[ $${SHALLOWEST_SOURCE#$$BAZEL_WORKSPACE} == src/thrift/* ]]; then
         |  ABSOLUTE_PREFIX="$$BAZEL_WORKSPACE/src/thrift"
         |elif [[ $${SHALLOWEST_SOURCE#$$BAZEL_WORKSPACE} == tests/thrift/* ]]; then
         |  ABSOLUTE_PREFIX="$$BAZEL_WORKSPACE/tests/thrift"
         |else
         |  LAST_THRIFT=$$MIN_DEPTH
         |  INDEX=2
         |  for PART in $$(echo "$$SHALLOWEST_SOURCE" | tr '/' ' '); do
         |    if [ "$$PART" == "thrift" ]; then
         |      LAST_THRIFT=$$INDEX
         |    fi
         |    INDEX=$$((INDEX + 1))
         |  done
         |  ABSOLUTE_PREFIX="$$(echo "$$SHALLOWEST_SOURCE" | cut -d'/' -f"-$$LAST_THRIFT")"
         |fi
         |
         |ZIP_TREE="$$(mktemp -d)"
         |
         |while [ $$# -gt 0 ]; do
         |  if [[ $$1 == *.srcjar || $$1 == *.jar ]]; then
         |    shift
         |  else
         |    PATH_WITHIN_ZIP="$${1#$$ABSOLUTE_PREFIX}"
         |    mkdir -p "$$(dirname "$$ZIP_TREE/$$PATH_WITHIN_ZIP")"
         |    cp "$$1" "$$ZIP_TREE/$$PATH_WITHIN_ZIP"
         |    shift
         |  fi
         |done
         |
         |cd "$$ZIP_TREE"
         |rm -f "$$SCROOGE_SOURCES_JAR"
         |zip -qqr "$$SCROOGE_SOURCES_JAR" *
         |cd - >/dev/null
         |
         |$scroogeWorker "@$$SCROOGE_INPUT"
         |
         |rm -rf "$$OUTPUT_DIRECTORY"
         |unzip -qq "$$SCROOGE_OUTPUT" -d "$$OUTPUT_DIRECTORY"
         |""".stripMargin
    Files.write(scriptLocation.toNIO, content.getBytes("UTF-8"))
    Files.setPosixFilePermissions(
      scriptLocation.toNIO,
      Set(
        PosixFilePermission.GROUP_EXECUTE,
        PosixFilePermission.GROUP_READ,
        PosixFilePermission.OTHERS_EXECUTE,
        PosixFilePermission.OTHERS_READ,
        PosixFilePermission.OWNER_EXECUTE,
        PosixFilePermission.OWNER_READ,
        PosixFilePermission.OWNER_WRITE
      ).asJava
    )
    scriptLocation
  }

  private def resources(
      target: Target,
      projectPackage: String
  ): Option[List[Path]] =
    target match {
      case ResourcesTarget() =>
        // Infer resources root.
        // See https://github.com/bazelbuild/rules_scala/blob/6c16cff213b76a4126bdc850956046da5db1daaa/scala/private/rule_impls.bzl#L55
        val resourcesRoot =
          Bazel.getAttribute(target, "resource_strip_prefix") match {
            case Some(prefix) if prefix.getExplicitlySpecified() =>
              projectPackage.stripPrefix(prefix.getStringValue())
            case _ =>
              def stopAfter(
                  haystack: String,
                  needle: String
              ): Option[String] = {
                val index = haystack.indexOf(needle)
                if (index == -1) None
                else Some(haystack.substring(0, index + needle.length))
              }
              stopAfter(projectPackage, "resources")
                .orElse(stopAfter(projectPackage, "java"))
                .getOrElse(projectPackage)
          }
        Some(List(project.common.workspace.resolve(resourcesRoot)))
      case _ =>
        None
    }

  private def javaConfig(
      target: Target,
      inputsMapping: CopiedJars
  ): Config.Platform.Jvm = {
    val compileJvmConfig = Config.JvmConfig(
      home = Some(Jdk.javaHome(target, Jdk.Compile)),
      options = javaOptions(target)
    )
    val runtimeJvmConfig = Config.JvmConfig(
      home = Some(Jdk.javaHome(target, Jdk.Runtime)),
      options = javaOptions(target)
    )
    Config.Platform.Jvm(
      config = compileJvmConfig,
      mainClass = mainClass(target),
      runtimeConfig = Some(runtimeJvmConfig),
      classpath = Some(runtimeClasspath(inputsMapping, target)),
      resources = None
    )
  }

  private def bloopProject(
      inputsMapping: CopiedJars,
      target: Target,
      projectDirectory: Path
  ): Config.Project = {
    val projectName = BloopBazel.bloopName(target)
    val projectPackage = projectName.takeWhile(_ != ':')
    val (sources, sourcesGlobs) =
      targetSourcesAndGlobs(target, projectDirectory)
    val deps = dependencies(target).map(BloopBazel.bloopName)
    val (javaSources, javaSourcesGlobs) =
      sourcesInfo.get(target).map(_.javaSources).getOrElse(Nil) match {
        case Nil => (Nil, Nil)
        case labels =>
          val (sources, globs) = labels.map(resolveJavaSources).unzip
          (sources.flatten, globs.flatten)
      }
    val targetDir = BloopBazel.targetDirectory(project, projectName)
    val jvmPlatform = javaConfig(target, inputsMapping)
    val cp = classpath(inputsMapping, target)

    // Fork the compilation to avoid running out of file descriptors
    // in very large classpaths.
    // This should not be necessary anymore when our Bazel rules will
    // observe `strict_deps`.
    val forceFork = cp.length > 2000
    val allGlobs = sourcesGlobs
      .map(globs =>
        globs.bloopConfig(project.common.workspace, projectDirectory)
      )
      .toList ++ javaSourcesGlobs

    Config.Project(
      name = projectName,
      directory = projectDirectory,
      workspaceDir = Some(project.common.workspace),
      sources = sources.map(project.common.workspace.resolve(_)) ++ javaSources,
      sourcesGlobs = Some(allGlobs).filter(_.nonEmpty),
      sourceRoots = approximateSourceRoot(projectDirectory).map(_ :: Nil),
      dependencies = deps,
      classpath = cp,
      out = targetDir.resolve("out").toNIO,
      classesDir = targetDir.resolve("classes").toNIO,
      resources = resources(target, projectPackage),
      `scala` = scala(target),
      java = Some(java(target, forceFork)),
      sbt = None,
      test = Some(test(target)),
      platform = Some(jvmPlatform),
      resolution = resolution(inputsMapping.sourceJars),
      tags = Some(List(if (isTest(target)) Tag.Test else Tag.Library)),
      sourceGenerators =
        sourceGenerators(inputsMapping, target, projectDirectory)
    )
  }

  // Returns a Bloop project that has no source code. This project only exists
  // to control for example how the project view is displayed in IntelliJ.
  private def emptyBloopProject(
      name: String,
      directory: Path
  ): Config.Project = {
    val targetDir = BloopBazel.targetDirectory(project, name)

    val emptyProject = Config.Project(
      name = name,
      directory = directory,
      workspaceDir = Some(bazelInfo.workspace.toNIO),
      sources = Nil,
      sourcesGlobs = None,
      sourceRoots = Some(directory :: Nil),
      dependencies = Nil,
      classpath = Nil,
      out = targetDir.resolve("out").toNIO,
      classesDir = targetDir.resolve("classes").toNIO,
      resources = None,
      scala = None,
      java = None,
      sbt = None,
      test = None,
      platform = None,
      resolution = None,
      tags = None,
      sourceGenerators = None
    )

    addDsStoreWorkaround(emptyProject)
  }

  private def addDsStoreWorkaround(project: Config.Project): Config.Project = {
    // Try to create `.DS_Store` which will be used to have IntelliJ consider the directory
    // as source root.
    try Files.newOutputStream(project.directory.resolve(".DS_Store")).close()
    catch { case NonFatal(_) => () }

    project.copy(
      // NOTE(mduhem): Create and tell Bloop to consider as sources `.DS_Store` in the target root
      // directory. Having at least a matching source is required for IntelliJ to consider the
      // directory as content root.
      sourcesGlobs = Some(
        PantsGlobs(".DS_Store" :: Nil, Nil).bloopConfig(
          bazelInfo.workspace.toNIO,
          project.directory
        ) :: project.sourcesGlobs.getOrElse(Nil)
      )
    )
  }

  private val producedResolution = new AtomicBoolean(false)
  private def resolution(sourceJars: List[Path]): Option[Config.Resolution] = {
    if (producedResolution.getAndSet(true)) None
    else {
      Some(
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
      )
    }
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

  private def java(target: Target, forceFork: Boolean) = {
    val baseOptions = List(
      // Default to `-implicit:none` but allow user to override, to mimic Bazel's behavior.
      // See https://github.com/bazelbuild/bazel/blob/e51a15f4395d4223c9665e5cc8ae2c8dd29e8f20/src/java_tools/buildjar/java/com/google/devtools/build/buildjar/JavaLibraryBuildRequest.java#L409-L410
      "-implicit:none"
    )

    val userOptions = Bazel
      .getAttribute(target, "javacopts")
      .map(_.getStringListValueList().asScala.toList)
      .getOrElse(Nil)
      // Filter out ErrorProne (https://errorprone.info) flags for now, since we don't have the plugin.
      .filterNot(_.startsWith("-Xep"))
      // Filter out Bazel specific options
      .filterNot(_.startsWith("-Werror:"))

    // Passing flags for the runtime system (`-J`) will cause Bloop to fork
    // Java compilation.
    val forkOptions =
      if (forceFork) List("-J-Dbloop.force.fork=fork") else Nil

    Config.Java(
      options = baseOptions ++ userOptions ++ forkOptions
    )
  }

  private def scala(target: Target): Option[Config.Scala] = {
    val options = Bazel
      .getAttribute(target, "scalacopts")
      .map(
        _.getStringListValueList().asScala.toList
      )

    options match {
      // Make sure we configure Scala for Thrift targets
      case None if target.getRule.getRuleClass != "scrooge_scala_library" =>
        None
      case opts =>
        Some(
          Config.Scala(
            organization = "org.scala-lang",
            name = "scala-compiler",
            version = "2.12.15",
            options = opts.getOrElse(Nil),
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
        )
    }
  }

  private def mainClass(target: Target): Option[String] =
    target match {
      case JvmAppTarget(_, bin) =>
        mainClass(bin)
      case _ =>
        Bazel
          .getAttribute(target, "main_class")
          .map(_.getStringValue())
          .filter(_.nonEmpty)
    }

  private val rawOutputToTarget: Map[Artifact, Target] = {
    importedTargets.flatMap {
      case ThriftTarget(target, root, "scala") =>
        actionGraph
          .outputsOfMnemonic(root.getRule().getName(), "Scalac")
          .map(_ -> target)
      case ThriftTarget(target, root, "java") =>
        actionGraph
          .outputsOfMnemonic(root.getRule().getName(), "Javac")
          .map(_ -> target)
      case target =>
        actionGraph
          .outputsOf(target.getRule.getName())
          .map(_ -> target)
    }.toMap
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

  private val targetsMap: Map[String, Target] =
    importedTargets
      .map(target => target.getRule.getName -> target)
      .toMap

  private def targetSourcesAndGlobs(
      target: Target,
      projectDirectory: Path
  ): (List[String], Option[PantsGlobs]) =
    target match {
      case ResourcesTarget() => (Nil, None)
      case _ =>
        val infos = sourcesInfo.get(target)
        val pantsGlobs = infos.map(i => PantsGlobs(i.include, i.exclude))
        pantsGlobs match {
          case Some(globs) if globs.isEmpty =>
            // Finding empty globs means that buildozer was able to extract the target from
            // the build file, but no sources were set. This means we need to use the default
            // globs.
            (Nil, Some(defaultGlobs(target)))
          case Some(globs) =>
            // Globs may need to be rebased if the target was rebased to avoid base directory
            // conflicts.
            (Nil, Some(rebaseGlobs(globs, target, projectDirectory)))
          case None =>
            // If the target doesn't appear in `targetGlobs`, it means that buildozer was not
            // able to read it from the build file. It's probably generated by a macro, so we
            // hardcode only the sources that Bazel knows about.
            val sources = Bazel
              .getAttribute(target, "srcs")
              .map(_.getStringListValueList().asScala.toList.map {
                _.replaceAllLiterally(":", File.separator).stripPrefix("//")
              })
              .getOrElse(Nil)
            (sources, None)
        }
    }

  private def resolveJavaSources(
      label: String
  ): (List[Path], List[Config.SourcesGlobs]) = {
    val packageName = label.takeWhile(_ != ':')
    val baseDirectory = project.common.workspace
      .resolve(packageName.stripPrefix("//"))

    // The label may be given in short form (foo/bar, which means foo/bar:bar)
    if (!label.contains(":")) {
      val targetName = baseDirectory.getFileName().toString()
      resolveJavaSources(s"$label:$targetName")
    } else {
      importedTargets.find(_.getRule.getName == label) match {
        // The target is a target that we imported, we can simply reuse the information
        case Some(target) =>
          val targetBase =
            project.common.workspace.resolve(Bazel.enclosingPackage(target))
          val (sources, pantsGlobs) = targetSourcesAndGlobs(target, targetBase)
          (
            sources.map(baseDirectory.resolve(_)),
            pantsGlobs.toList.map(
              _.bloopConfig(project.common.workspace, baseDirectory)
            )
          )

        // The target is not an imported target, we need to find this information
        case None =>
          val absoluteLabel =
            if (label.startsWith("//")) label else "//" + label
          bazel
            .groupedSourcesGlobs(packageName :: Nil)
            .get(absoluteLabel) match {
            case None =>
              (Nil, Nil)
            case Some(SourcesInfo(Nil, _, _)) =>
              // Finding empty includes means that we need to use the default globs
              (
                Nil,
                defaultJavaGlobs.bloopConfig(
                  project.common.workspace,
                  baseDirectory
                ) :: Nil
              )
            case Some(SourcesInfo(include, exclude, _)) =>
              (
                Nil,
                PantsGlobs(include, exclude)
                  .bloopConfig(project.common.workspace, baseDirectory) :: Nil
              )
          }
      }
    }
  }

  private def isTest(target: Target): Boolean = {
    target.getRule().getRuleClass() == "scala_junit_test"
  }

  private def rebaseGlobs(
      globs: PantsGlobs,
      target: Target,
      baseDirectory: Path
  ): PantsGlobs = {
    val projectName = BloopBazel.bloopName(target)
    val projectPackage = projectName.takeWhile(_ != ':')
    val realBaseDirectory = project.common.workspace.resolve(projectPackage)
    if (baseDirectory.startsWith(realBaseDirectory)) {
      val toStrip = realBaseDirectory.relativize(baseDirectory).toString + "/"
      globs.copy(
        include = globs.include.map(_.stripPrefix(toStrip)),
        exclude = globs.exclude.map(_.stripPrefix(toStrip))
      )
    } else globs
  }

  private def defaultGlobs(target: Target): PantsGlobs = {
    Bazel.generatorFunction(target) match {
      case Some("scala_library") => PantsGlobs("*.scala" :: Nil, Nil)
      case Some("java_library") => defaultJavaGlobs
      case Some("junit_tests") =>
        PantsGlobs("*Test.scala" :: "*Spec.scala" :: "*Test.java" :: Nil, Nil)
      case Some("create_thrift_libraries") => PantsGlobs("*.thrift" :: Nil, Nil)
      case _ => PantsGlobs.empty
    }
  }

  private val sourceRootPattern = FileSystems.getDefault.getPathMatcher(
    "glob:**/{main,test,tests,src,3rdparty,3rd_party,thirdparty,third_party}/{resources,scala,java,jvm,proto,python,protobuf,py}"
  )
  private val defaultTestRootPattern = FileSystems.getDefault.getPathMatcher(
    "glob:**/{test,tests}"
  )

  private val defaultJavaGlobs = PantsGlobs("*.java" :: Nil, Nil)

  private def approximateSourceRoot(dir: Path): Option[Path] = {
    @tailrec def loop(d: Path): Option[Path] = {
      if (sourceRootPattern.matches(d)) Some(d)
      else if (defaultTestRootPattern.matches(d)) Some(d)
      else {
        Option(d.getParent) match {
          case Some(parent) => loop(parent)
          case None => None
        }
      }
    }
    loop(dir)
  }

  private def copyJars(
      bazelInfo: BazelInfo,
      project: Project,
      importedTargets: List[Target],
      actionGraph: ActionGraph,
      rawTargetInputs: Map[Target, List[Artifact]],
      rawRuntimeTargetInputs: Map[Target, List[Artifact]]
  ): Try[CopiedJars] = {
    val fromOutputs = importedTargets.flatMap {
      case ThriftTarget(target, root, mnemonic) =>
        actionGraph.outputsOfMnemonic(root.getRule().getName(), mnemonic).map {
          output =>
            output -> BloopBazel
              .targetDirectory(project, BloopBazel.bloopName(target))
              .resolve("classes")
              .toNIO
        }
      case ThriftRoot(root, scala, java) =>
        Nil
      case target =>
        actionGraph.outputsOf(target.getRule.getName).map { output =>
          output -> BloopBazel
            .targetDirectory(project, BloopBazel.bloopName(target))
            .resolve("classes")
            .toNIO
        }
    }.toMap
    //
    // Find out the artifacts produced by the platform classpath target, so that we can exclude them
    // from the classpaths of all targets. Bloop will configure the right ones from the environments.
    // Leaving the artifacts provided by Bazel would break go to definition on JDK classes.
    val platformClasspathArtifacts =
      actionGraph.outputsOf(Bazel.PlatformClasspathLabel)

    val allInputs =
      rawTargetInputs.valuesIterator.flatten.toSet ++
        rawRuntimeTargetInputs.valuesIterator.flatten.toSet --
        fromOutputs.keySet --
        platformClasspathArtifacts

    val fromInputs =
      BloopBazel.copyImmutableJars(
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

  private def isThriftTarget(target: Target): Boolean = {
    val ruleClass = target.getRule.getRuleClass
    ruleClass == "thrift_library" || ruleClass == "scrooge_scala_library" || ruleClass == "scrooge_java_library"
  }

}

private case class CopiedJars(
    artifactToPath: Map[Artifact, Path],
    sourceJars: List[Path]
)
