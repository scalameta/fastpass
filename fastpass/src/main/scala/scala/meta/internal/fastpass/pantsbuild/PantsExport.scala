package scala.meta.internal.fastpass.pantsbuild

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.{util => ju}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

import scala.meta.io.AbsolutePath

import ujson.Bool
import ujson.Obj
import ujson.Str

case class PantsExport(
    targets: collection.Map[String, PantsTarget],
    librariesJava: ju.HashMap[String, PantsLibrary],
    internalSourcesDir: Path,
    scalaPlatform: PantsScalaPlatform,
    jvmDistribution: PantsPreferredJvmDistribution
) {
  def libraries = librariesJava.asScala
}

object PantsExport {
  def fromJson(args: Export, output: ujson.Value): PantsExport = {
    val allTargets = output.obj("targets").obj
    val targetsByDirectory = allTargets.keys.groupBy { name =>
      PantsConfiguration.baseDirectoryString(name)
    }
    val jvmPlatforms: Map[String, String] =
      output.obj.get(PantsKeys.preferredJvmDistributions) match {
        case Some(obj: Obj) =>
          (for {
            (key, value) <- obj.value
            strict <- value.obj.get(PantsKeys.strict)
          } yield key -> strict.str).toMap
        case _ => Map.empty
      }
    val targets = new ju.HashMap[String, PantsTarget]
    val internalSourcesDir =
      Files.createDirectories(args.bloopDir.resolve("sources-jar"))
    for {
      (name, valueObj) <- allTargets.iterator
    } {
      val value = valueObj.obj
      val directDependencies = value(PantsKeys.dependencies).arr.map(_.str)
      val syntheticDependencies: Iterable[String] =
        if (args.isMergeTargetsInSameDirectory) {
          targetsByDirectory
            .getOrElse(
              PantsConfiguration.baseDirectoryString(name),
              Nil
            )
            .filterNot(_ == name)
        } else {
          Nil
        }
      val dependencies = directDependencies ++ syntheticDependencies
      val javaSources = asStringList(value, PantsKeys.javaSources)
      val excludes = new ju.HashSet[String]
      for {
        exclude <- value.get(PantsKeys.excludes).iterator
        value <- exclude.arr.iterator
      } {
        excludes.add(value.str)
      }
      val platform = for {
        platform <- value.get(PantsKeys.platform)
        javaHome <- jvmPlatforms.get(platform.str)
      } yield javaHome
      val libraries: mutable.ArrayBuffer[String] =
        value(PantsKeys.libraries).arr.map(_.str.intern())
      val compileLibraries: mutable.ArrayBuffer[String] = value
        .getOrElse(PantsKeys.compileLibraries, value(PantsKeys.libraries))
        .arr
        .map(_.str.intern())
      val runtimeLibraries: mutable.ArrayBuffer[String] = value
        .getOrElse(PantsKeys.runtimeLibraries, value(PantsKeys.libraries))
        .arr
        .map(_.str.intern())
      val isPantsTargetRoot = value(PantsKeys.isTargetRoot).bool
      val isPantsModulizable = value(PantsKeys.isModulizable).bool
      val pantsTargetType =
        PantsTargetType(value(PantsKeys.pantsTargetType).str)
      val targetType =
        if (pantsTargetType.isNodeModule) {
          // NOTE(olafur) Treat "node_module" targets as `target_type:
          // RESOURCE` since they are included on the runtime classpath even
          // if they have `target_type: SOURCE`. See
          // https://github.com/pantsbuild/pants/issues/9026 for a reason why
          // node_module needs special handling.
          TargetType("RESOURCE")
        } else {
          TargetType(value(PantsKeys.targetType).str)
        }
      val id = value(PantsKeys.id).str
      val directoryName = BloopPants.makeClassesDirFilename(id)
      val classesDir: Path = Files.createDirectories(
        args.bloopDir.resolve(directoryName).resolve("classes")
      )
      val internalSourcesJar = internalSourcesDir.resolve(id + "-sources.jar")
      val baseDirectory = PantsConfiguration
        .baseDirectory(AbsolutePath(args.workspace), name)
        .toNIO
      val target = new PantsTarget(
        name = name,
        id = id,
        dependencies = dependencies,
        javaSources = javaSources,
        excludes = excludes.asScala,
        platform = platform,
        libraries = libraries,
        isPantsTargetRoot = isPantsTargetRoot,
        isPantsModulizable = isPantsModulizable,
        targetType = targetType,
        pantsTargetType = pantsTargetType,
        globs = PantsGlobs.fromJson(value),
        roots = PantsRoots.fromJson(value),
        scalacOptions = asStringList(value, PantsKeys.scalacArgs),
        javacOptions = asStringList(value, PantsKeys.javacArgs),
        extraJvmOptions = asStringList(value, PantsKeys.extraJvmOptions),
        directoryName = directoryName,
        baseDirectory = baseDirectory,
        classesDir = classesDir,
        internalSourcesJar = internalSourcesJar,
        isSynthetic = asBoolean(value, PantsKeys.isSynthetic),
        strictDeps = asBoolean(value, PantsKeys.strictDeps),
        exports = asStringList(value, PantsKeys.exports).toSet,
        scope = PantsScope.fromJson(value),
        targetBase = asString(value, PantsKeys.target_base),
        mainClass = asString(value, PantsKeys.main_class)
      )
      targets.put(name, target)
    }

    val allLibraries = output.obj(PantsKeys.libraries).obj
    val libraries = new ju.HashMap[String, PantsLibrary]
    for {
      (libraryNameNonInterned, valueObj) <- allLibraries.iterator
    } {
      val libraryName = libraryNameNonInterned.intern()
      // The "$ORGANIZATION:$ARTIFACT" part of Maven library coordinates.
      val module = {
        val colon = libraryName.lastIndexOf(':')
        if (colon < 0) libraryName
        else libraryName.substring(0, colon)
      }
      val values = valueObj.obj.flatMap {
        case (key, value) =>
          val path = Paths.get(value.str)
          if (Files.exists(path)) Some(key -> path)
          else None
      }
      libraries.put(
        libraryName,
        PantsLibrary(libraryName, module, values)
      )
    }

    val scalaPlatform = PantsScalaPlatform.fromJson(output)

    val jvmDistribution = PantsPreferredJvmDistribution.fromJson(output.obj)

    PantsExport(
      targets = targets.asScala,
      librariesJava = libraries,
      internalSourcesDir = internalSourcesDir,
      scalaPlatform = scalaPlatform,
      jvmDistribution = jvmDistribution
    )
  }

  private def asBoolean(obj: Obj, key: String): Boolean =
    obj.value.get(key) match {
      case Some(Bool(true)) => true
      case _ => false
    }

  private def asString(obj: Obj, key: String): Option[String] =
    obj.value.get(key).collect {
      case Str(value) => value
    }

  private def asStringList(obj: Obj, key: String): List[String] =
    obj.value.get(key) match {
      case None => Nil
      case Some(value) =>
        value.arr.iterator.collect {
          case Str(value) => value
        }.toList
    }

}
