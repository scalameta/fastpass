package scala.meta.internal.fastpass.pantsbuild

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption
import java.util.concurrent.ConcurrentHashMap
import java.{util => ju}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.sys.process.Process
import scala.util.Failure
import scala.util.Properties
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal

import scala.meta.internal.fastpass.BuildInfo
import scala.meta.internal.fastpass.FastpassEnrichments._
import scala.meta.internal.fastpass.FileUtils
import scala.meta.internal.fastpass.InterruptException
import scala.meta.internal.fastpass.SystemProcess
import scala.meta.internal.fastpass.generic.DependencyResolution
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath

import bloop.config.Tag
import bloop.config.{Config => C}
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import ujson.Value

object BloopPants {
  def pantsOwnerOf(
      workspace: AbsolutePath,
      source: AbsolutePath
  ): Seq[String] = {
    try {
      val relpath = source.toRelative(workspace).toString()
      val output = Process(
        List[String](
          workspace.resolve("pants").toString(),
          "--concurrent",
          s"--owner-of=$relpath",
          "list"
        ),
        cwd = Some(workspace.toFile)
      ).!!
      output.linesIterator.toSeq.distinct
    } catch {
      case NonFatal(_) =>
        Nil
    }
  }

  private def targetDirectory(target: String): String = {
    val colon = target.lastIndexOf(':')
    if (colon < 0) target
    else target.substring(0, colon)
  }

  private def interruptedTry[T](thunk: => T): Try[T] =
    try {
      Success(thunk)
    } catch {
      case NonFatal(e) => Failure(e)
      case e @ InterruptException() => Failure(e)
    }

  def bloopInstall(
      args: Export
  )(implicit ec: ExecutionContext): Try[PantsExportResult] =
    interruptedTry {
      val cacheDir = Files.createDirectories(
        args.workspace.resolve(".pants.d").resolve("metals")
      )
      val outputFile = cacheDir.resolve(s"${args.project.name}-export.json")
      val bloopDir = args.bloopDir
      if (Files.isSymbolicLink(bloopDir)) {
        Files.delete(bloopDir)
      }
      Files.createDirectories(bloopDir)
      args.token.checkCanceled()

      def readJson(file: Path): Option[Value] = {
        Try {
          val text =
            new String(Files.readAllBytes(outputFile), StandardCharsets.UTF_8)
          ujson.read(text)
        }.toOption
      }
      val fromCache: Option[Value] =
        if (!args.isCache) None
        else readJson(outputFile)
      val fromExport: Option[Value] =
        fromCache.orElse {
          runPantsExport(args, outputFile)
          readJson(outputFile)
        }

      fromExport match {
        case Some(value) =>
          val export = PantsExport.fromJson(args, value)
          new BloopPants(args, bloopDir, export).run()
        case None =>
          throw new NoSuchFileException(
            outputFile.toString(),
            null,
            "expected this file to exist after running `./pants export`"
          )
      }
    }

  private def runPantsExport(
      args: Export,
      outputFile: Path
  )(implicit ec: ExecutionContext): Unit = {
    val noInternalSources =
      if (args.sources.isOff || args.sources.isOnDemand) "no-"
      else ""
    val no3rdPartySources = if (args.sources.isOff) "no-" else ""

    val command = List[String](
      args.common.pantsBinary.toString(),
      "--concurrent",
      s"--no-quiet",
      s"--${noInternalSources}export-fastpass-sources",
      s"--${no3rdPartySources}export-fastpass-libraries-sources",
      s"--export-fastpass-output-file=$outputFile",
      s"--export-fastpass-allow-global-excludes=False",
      s"export-fastpass"
    ) ++ args.targets
    val shortName = "pants export-fastpass"
    val bloopSymlink = args.workspace.resolve(".bloop")
    val bloopSymlinkTarget =
      if (Files.isSymbolicLink(bloopSymlink)) {
        Some(Files.readSymbolicLink(bloopSymlink))
      } else {
        None
      }
    try {
      // NOTE(olafur): Delete `.bloop` symbolic link while doing `./pants
      // export-dep-as-jar` because the symbolic link can trigger errors in
      // Pants. The symbolic link is recovered in the finally block after the
      // export command completes.
      bloopSymlinkTarget.foreach(_ => Files.deleteIfExists(bloopSymlink))
      SystemProcess.run(
        shortName,
        command,
        command,
        args.workspace,
        args.token,
        args.app.out,
        args.app.err
      )
    } finally {
      bloopSymlinkTarget.foreach { target =>
        if (!Files.exists(bloopSymlink)) {
          Files.createSymbolicLink(bloopSymlink, target)
        }
      }
    }
  }

  private val sourceRootPattern = FileSystems.getDefault.getPathMatcher(
    "glob:**/{main,test,tests,src,3rdparty,3rd_party,thirdparty,third_party}/{resources,scala,java,jvm,proto,python,protobuf,py}"
  )
  private val defaultTestRootPattern = FileSystems.getDefault.getPathMatcher(
    "glob:**/{test,tests}"
  )

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

}

private class BloopPants(
    args: Export,
    bloopDir: Path,
    export: PantsExport
)(implicit ec: ExecutionContext) { self =>
  val size: Int = export.targets.valuesIterator.count(_.isModulizable)
  val isLarge: Boolean = size > 250
  def token: CancelChecker = args.token
  def workspace: Path = args.workspace
  def userTargets: List[String] = args.targets

  private val scalaLibrary = "org.scala-lang:scala-library:"
  private val transitiveClasspath = new ju.HashMap[String, List[Path]]().asScala
  private val isVisited = mutable.Set.empty[String]

  val scalaVersion: String = export.libraries.keysIterator
    .collectFirst {
      case module if module.startsWith(scalaLibrary) =>
        module.stripPrefix(scalaLibrary)
    }
    .getOrElse {
      scribe.warn(
        s"missing scala-library: falling back to ${Properties.versionNumberString}"
      )
      Properties.versionNumberString
    }

  private val mutableJarsHome = workspace.resolve(".pants.d")
  private val bloopJars =
    Files.createDirectories(bloopDir.resolve("bloop-jars"))
  private val internalSourcesJars =
    Files.createDirectories(bloopDir.resolve("sources-jars"))
  private val toCopyBuffer = new ju.HashMap[Path, Path]()
  private val exportClasspaths =
    new ju.HashMap[PantsTarget, List[Path]]().asScala
  private val jarPattern =
    FileSystems.getDefault().getPathMatcher("glob:**.jar")
  private val copiedJars = new ju.HashSet[Path]()
  private val resolutionLibraries = ju.Collections.newSetFromMap(
    new ju.IdentityHashMap[PantsLibrary, java.lang.Boolean]
  )
  private val resolutionTargets = ju.Collections.newSetFromMap(
    new ju.IdentityHashMap[PantsTarget, java.lang.Boolean]
  )
  val allScalaJars: List[Path] =
    export.scalaPlatform.compilerClasspath
      .map(jar => toImmutableJar(jar.getFileName.toString, jar))
      .toList

  def run(): PantsExportResult = {
    token.checkCanceled()
    val projects = export.targets.valuesIterator
      .filter(_.isModulizable)
      .zipWithIndex
      .map {
        case (target, i) =>
          printProgress(i)
          toBloopProject(target)
      }
      .toList
    copyImmutableJars()
    val internalSources = generateInternalSourcesJars()
    val sourceRoots = PantsConfiguration.sourceRoots(
      AbsolutePath(workspace),
      args.targets
    )
    val isBaseDirectory =
      projects.iterator.filter(_.sourcesGlobs.nonEmpty).map(_.directory).toSet
    // NOTE(olafur): generate synthetic projects to improve the file tree view
    // in IntelliJ. Details: https://github.com/olafurpg/intellij-bsp-pants/issues/7
    val syntheticProjects: List[C.Project] = sourceRoots.flatMap { root =>
      if (isBaseDirectory(root.toNIO)) {
        Nil
      } else {
        val name = root
          .toRelative(AbsolutePath(workspace))
          .toURI(isDirectory = false)
          .toString()
        // NOTE(olafur): cannot be `name + "-root"` since that conflicts with the
        // IntelliJ-generated root project.
        List(toEmptyBloopProject(name + "-project-root", root.toNIO))
      }
    }
    val generatedProjects = new mutable.LinkedHashSet[Path]
    val allProjects = syntheticProjects ::: projects
    val byName = allProjects.map(p => p.name -> p).toMap
    allProjects.foreach { project =>
      val finalProject = project
      val id = export.targets.get(finalProject.name).fold(project.name)(_.id)
      val out = bloopDir.resolve(FileUtils.makeJsonFilename(id))
      val json = C.File(BuildInfo.bloopVersion, finalProject)
      bloop.config.write(json, out)
      generatedProjects += out
    }
    cleanStaleBloopFiles(generatedProjects)
    token.checkCanceled()
    new PantsExportResult(
      generatedProjects.size,
      0,
      internalSources,
      export.jvmDistribution.javaHome,
      export.libraries.valuesIterator
    )
  }

  val intervals = 20
  val points: Int = 100 / intervals
  val isProgressPoint: Map[Int, Int] = 0
    .until(size)
    .by(math.max(1, size / (intervals - 1)))
    .zipWithIndex
    .toMap
    .updated(size - 1, intervals)
  def printProgress(i: Int): Unit = {
    if (isLarge) {
      isProgressPoint.get(i).foreach { step =>
        val hashes = "#" * step
        val percentage = step * points
        args.app.info(
          f"$percentage%3s%% [$hashes%-20s] ${size - i - 1}%4s targets remaining"
        )
      }
    }
  }

  def getSources(target: PantsTarget): List[Path] = {
    def sourcesForSingleTarget(t: PantsTarget): List[Path] =
      if (t.isGeneratedTarget) t.roots.sourceRoots
      else Nil
    val targetSources = sourcesForSingleTarget(target)
    val javaSources = (for {
      javaSourcesName <- target.javaSources.iterator
      sources <- sourcesForSingleTarget(export.targets(javaSourcesName))
    } yield sources).toList
    targetSources ++ javaSources
  }

  def getSourcesGlobs(
      target: PantsTarget,
      baseDirectory: Path
  ): Option[List[C.SourcesGlobs]] = {
    def sourceGlobsForSingleTarget(
        t: PantsTarget,
        baseDir: Path
    ): List[C.SourcesGlobs] =
      if (t.targetType.isResourceOrTestResource) Nil
      else List(t.globs.bloopConfig(workspace, baseDir))
    val targetSourcesGlobs = sourceGlobsForSingleTarget(target, baseDirectory)
    val javaSourcesGlobs = (for {
      javaSourcesName <- target.javaSources.iterator
      javaSources = export.targets(javaSourcesName)
      globs <-
        sourceGlobsForSingleTarget(javaSources, javaSources.baseDirectory)
    } yield globs).toList
    Option(targetSourcesGlobs ++ javaSourcesGlobs).filter(_.nonEmpty)
  }

  val runtime = new RuntimeBFS(export, RuntimeScope)
  val compile = new CompileBFS(export, args.strictDeps)

  def computeClasspath(
      target: PantsTarget,
      transitiveDependencies: Iterable[PantsTarget],
      libraries: Iterable[PantsLibrary]
  ): List[Path] = {
    val classpathEntries = new ju.LinkedHashSet[Path]
    for {
      dependency <- transitiveDependencies.iterator
    } {
      if (dependency.isModulizable) {
        classpathEntries.add(dependency.classesDir)
      } else {
        classpathEntries.addAll(toImmutableJars(dependency).asJava)
      }
    }

    for {
      library <- libraries.iterator
      path <- library.nonSources
    } {
      classpathEntries.add(toImmutableJar(library, path))
    }

    classpathEntries.addAll(allScalaJars.asJava)
    if (target.targetType.isTest) {
      classpathEntries.addAll(
        DependencyResolution.testingFrameworkJars.getOrElse(Nil).asJava
      )
    }
    classpathEntries.iterator.asScala.toList
  }

  /**
   * Generates `*-sources.jar` files containing the sources of non-modulizable
   * target dependencies. For every such target, we expand the sources/globs
   * and compress the files into a jar file.
   */
  def generateInternalSourcesJars(): collection.Map[Path, Path] = {
    val result = new ConcurrentHashMap[Path, Path]
    resolutionTargets.stream().parallel().forEach { target =>
      target.targetBase.foreach { targetBase =>
        val sources = target.internalSourcesJar
        Files.deleteIfExists(sources)
        FileIO.withJarFileSystem(
          AbsolutePath(sources),
          create = true,
          close = true
        ) { root =>
          val sourceRoot = AbsolutePath(workspace).resolve(targetBase)
          val jars = new SourcesJarBuilder(root.toNIO)
          jars.writeSourceRoot(sourceRoot.toRelative(AbsolutePath(workspace)))
          getSources(target)
            .foreach(dir => jars.expandDirectory(AbsolutePath(dir), sourceRoot))
          getSourcesGlobs(target, target.baseDirectory).iterator.flatten
            .foreach(glob => jars.expandGlob(glob, sourceRoot))
        }
        toImmutableJars(target).headOption.foreach { default =>
          result.put(default, sources)
        }
      }
    }
    val isGenerated = result.asScala.valuesIterator.toSet
    garbageCollectUnusedJars(
      export.internalSourcesDir,
      isIncluded = path => jarPattern.matches(path),
      isExcluded = path => isGenerated(path)
    )
    result.asScala
  }

  private def garbageCollectUnusedJars(
      directory: Path,
      isIncluded: Path => Boolean,
      isExcluded: Path => Boolean
  ): Unit = {
    AbsolutePath(internalSourcesJars).list.foreach { absoluteJar =>
      val jar = absoluteJar.toNIO
      if (isIncluded(jar) && !isExcluded(jar)) {
        // Garbage collect unused jars.
        Files.deleteIfExists(jar)
      }
    }
  }

  def getResolution(
      dependencies: Iterable[PantsTarget],
      libraries: Iterable[PantsLibrary]
  ): Option[C.Resolution] = {
    if (args.sources.isOnDemand) return None
    // NOTE(olafur): avoid sending the same *-sources.jar to reduce the
    // size of the Bloop JSON configs. Both IntelliJ and Metals only need each
    // jar to appear once.
    val fromDeps = for {
      target <- dependencies.iterator
      if !target.isModulizable && !resolutionTargets.contains(target)
    } yield {
      resolutionTargets.add(target)
      newSourceModule(target.internalSourcesJar)
    }
    val fromLibs = for {
      library <- libraries.iterator
      if !resolutionLibraries.contains(library)
      source <- library.sources
    } yield {
      resolutionLibraries.add(library)
      newSourceModule(source)
    }
    Some(C.Resolution(Iterator(fromDeps, fromLibs).flatten.toList))
  }

  private def toBloopProject(target: PantsTarget): C.Project = {

    val baseDirectory = target.baseDirectory

    val sources = getSources(target)
    val sourcesGlobs = getSourcesGlobs(target, baseDirectory)

    val runtimeDependencies = runtime.dependencies(target)
    val compileDependencies = compile.dependencies(target)

    val dependencies =
      runtimeDependencies.iterator
        .filter(_.isModulizable)
        .map(_.dependencyName)
        .toList

    // With pants, the libraries that appear in `excludes` are still included on the compilation
    // classpath, but they are removed from the runtime classpath.
    val runtimeLibraries =
      classpathLibraries(target, runtimeDependencies, ignoreExcludes = false)
    val compileLibraries =
      classpathLibraries(target, compileDependencies, ignoreExcludes = true)

    val runtimeClasspath =
      computeClasspath(target, runtimeDependencies, runtimeLibraries)
    val compileClasspath =
      computeClasspath(target, compileDependencies, compileLibraries)

    val resolution = getResolution(runtimeDependencies, runtimeLibraries)

    val out: Path = bloopDir.resolve(target.directoryName)
    val classesDir = target.classesDir
    val javaHome: Option[Path] = target.platform.map(Paths.get(_))
    val runtimeJavaHome: Option[Path] = target.runtimePlatform.map(Paths.get(_))

    val resources: Option[List[Path]] =
      if (!target.targetType.isResourceOrTestResource) None
      else {
        target.targetBase.map { targetBase =>
          List(workspace.resolve(targetBase))
        }
      }

    val sourceRoots = target.targetBase.map { targetBase =>
      List(workspace.resolve(targetBase))
    }

    val tags =
      if (target.targetType.isTest) List(Tag.Test)
      else List(Tag.Library)

    // Pants' `extra_jvm_options` should apply only to test execution,
    // so we ignore them for non-test targets.
    val extraJvmOptions =
      if (target.targetType.isTest) target.extraJvmOptions else Nil

    val fullJvmOptions =
      List(s"-Duser.dir=$workspace") ++ extraJvmOptions

    C.Project(
      name = target.dependencyName,
      directory = baseDirectory,
      workspaceDir = Some(workspace),
      sources = sources,
      sourcesGlobs = sourcesGlobs,
      sourceRoots = sourceRoots,
      dependencies = dependencies,
      classpath = compileClasspath,
      out = out,
      classesDir = classesDir,
      resources = resources,
      scala = bloopScala(target.scalacOptions),
      java = Some(C.Java(target.javacOptions)),
      sbt = None,
      test = bloopTestFrameworks,
      platform = Some(
        C.Platform.Jvm(
          C.JvmConfig(
            javaHome,
            fullJvmOptions
          ),
          target.mainClass,
          runtimeJavaHome.map(_ =>
            C.JvmConfig(
              runtimeJavaHome,
              fullJvmOptions
            )
          ),
          Some(runtimeClasspath),
          None
        )
      ),
      resolution = resolution,
      tags = Some(tags)
    )
  }

  def classpathLibraries(
      target: PantsTarget,
      dependencies: Iterable[PantsTarget],
      ignoreExcludes: Boolean
  ): Iterable[PantsLibrary] = {
    val out = new mutable.ArrayBuffer[PantsLibrary]()
    val isVisited = new IdentityHashSet[String]
    val excludes =
      if (ignoreExcludes) Set.empty[String]
      else target.excludes ++ dependencies.iterator.flatMap(_.excludes)
    for {
      dependency <- dependencies.iterator
      libraryName <- dependency.libraries.iterator
    } {
      if (!isVisited.contains(libraryName)) {
        isVisited.add(libraryName)
        val library = export.librariesJava.get(libraryName)
        // Respect "excludes" setting in Pants BUILD files to exclude library dependencies.
        if (library != null && !excludes.contains(library.module)) {
          out += library
        }
      }
    }
    out
  }

  // Returns a Bloop project that has no source code. This project only exists
  // to control for example how the project view is displayed in IntelliJ.
  private def toEmptyBloopProject(name: String, directory: Path): C.Project = {
    val directoryName = FileUtils.makeClassesDirFilename(name)
    val classesDir: Path = Files.createDirectories(
      bloopDir.resolve(directoryName).resolve("classes")
    )
    C.Project(
      name = name,
      directory = directory,
      workspaceDir = Some(workspace),
      sources = Nil,
      sourcesGlobs = None,
      sourceRoots = None,
      dependencies = Nil,
      classpath = Nil,
      out = bloopDir.resolve(directoryName),
      classesDir = classesDir,
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

  private def toImmutableJars(target: PantsTarget): List[Path] = {
    exportClasspaths.getOrElseUpdate(
      target, {
        val classpathFile = AbsolutePath(
          workspace
            .resolve("dist")
            .resolve("export-fastpass")
            .resolve(s"${target.id}-classpath.txt")
        )
        if (!classpathFile.isFile) {
          Nil
        } else {
          val mutableClasspath = Classpath(classpathFile.readText.trim())
          mutableClasspath.entries.zipWithIndex.flatMap {
            case (alias, i) =>
              val entry = alias.dealias
              val suffix = if (i == 0) "" else s"-$i"
              if (entry.isDirectory) {
                List(entry.toNIO)
              } else if (entry.isFile) {
                List(
                  toImmutableJar(s"${target.id}$suffix.jar", entry.toNIO)
                )
              } else {
                Nil
              }
          }
        }
      }
    )
  }
  private def toImmutableJar(library: PantsLibrary, path: Path): Path = {
    val fromCache = toCopyBuffer.get(path)
    if (fromCache != null) fromCache
    else {
      val filename = FileUtils.makeReadableFilename(library.name) + ".jar"
      toImmutableJar(filename, path)
    }
  }
  private def toImmutableJar(filename: String, path: Path): Path = {
    // NOTE(olafur): Jars that live inside $WORKSPACE/.pants.d get overwritten
    // by Pants during compilation. We copy these jars over to the Bloop
    // directory so that the Bloop incremental cache is unaffected by the
    // `./pants compile` tasks that the user is running.
    if (path.startsWith(mutableJarsHome) && jarPattern.matches(path)) {
      toCopyBuffer.computeIfAbsent(
        path,
        _ => {
          val destination = bloopJars.resolve(filename)
          copiedJars.add(destination)
          destination
        }
      )
    } else {
      toCopyBuffer.put(path, path)
      path
    }
  }
  private def copyImmutableJars(): Unit = {
    toCopyBuffer.entrySet().parallelStream().forEach { entry =>
      val source = entry.getKey()
      val destination = entry.getValue()
      try {
        Files.copy(
          source,
          destination,
          StandardCopyOption.REPLACE_EXISTING
        )
      } catch {
        case e: IOException =>
          scribe.warn(s"failed to copy jar ${entry.getKey()}", e)
      }
    }
    garbageCollectUnusedJars(
      bloopJars,
      isIncluded = path => jarPattern.matches(path),
      isExcluded = path => copiedJars.contains(path)
    )
  }

  def bloopScala(scalacOptions: List[String]): Option[C.Scala] =
    Some(
      C.Scala(
        "org.scala-lang",
        "scala-compiler",
        scalaVersion,
        scalacOptions,
        allScalaJars,
        None,
        setup = Some(
          C.CompileSetup(
            C.Mixed,
            addLibraryToBootClasspath = true,
            addCompilerToClasspath = false,
            addExtraJarsToClasspath = false,
            manageBootClasspath = true,
            filterLibraryFromClasspath = true
          )
        )
      )
    )

  private def bloopTestFrameworks: Option[C.Test] = {
    Some(
      C.Test(
        frameworks = List(
          C.TestFramework(List("munit.internal.junitinterface.PantsFramework"))
        ),
        options = C.TestOptions(
          excludes = Nil,
          arguments = Nil
        )
      )
    )
  }

  private def newSourceModule(source: Path) =
    C.Module(
      "",
      "",
      "",
      None,
      artifacts = List(
        C.Artifact(
          "",
          classifier = Some("sources"),
          None,
          path = source
        )
      )
    )

  private def isScalaJar(module: String): Boolean =
    module.startsWith(scalaLibrary) ||
      module.startsWith("org.scala-lang:scala-reflect:") ||
      module.startsWith("org.scala-lang:scala-compiler:") ||
      module.startsWith("org.fursesource:jansi:") ||
      module.startsWith("jline:jline:")

  private def cleanStaleBloopFiles(
      generatedProjects: collection.Set[Path]
  ): Unit = {
    val jsonPattern = FileSystems.getDefault().getPathMatcher("glob:**/*.json")
    AbsolutePath(bloopDir).list
      .filter { path =>
        // Re-implementation of https://github.com/scalacenter/bloop/blob/e014760490bf140e2755eb91260bdaf9a75e4476/integrations/sbt-bloop/src/main/scala/bloop/integrations/sbt/SbtBloop.scala#L1064-L1079
        path.isFile &&
        jsonPattern.matches(path.toNIO) &&
        path.filename != "bloop.settings.json" &&
        !generatedProjects(path.toNIO)
      }
      .foreach { path => Files.deleteIfExists(path.toNIO) }
  }

}
