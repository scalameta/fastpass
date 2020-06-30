package scala.meta.internal.fastpass.pantsbuild

import java.nio.file.Path

// NOTE(olafur): intentionally not a case class to use reference equality.
class PantsTarget(
    val name: String,
    val id: String,
    val dependencies: collection.Seq[String],
    val javaSources: collection.Seq[String],
    val excludes: collection.Set[String],
    val platform: Option[String],
    val libraries: collection.Seq[String],
    val isPantsTargetRoot: Boolean,
    val isPantsModulizable: Boolean,
    val targetType: TargetType,
    val pantsTargetType: PantsTargetType,
    val globs: PantsGlobs,
    val roots: PantsRoots,
    val scalacOptions: List[String],
    val javacOptions: List[String],
    val extraJvmOptions: List[String],
    val directoryName: String,
    val baseDirectory: Path,
    val classesDir: Path,
    val internalSourcesJar: Path,
    val strictDeps: Boolean,
    val isSynthetic: Boolean,
    val exports: Set[String],
    val scope: PantsScope,
    val targetBase: Option[String],
    val mainClass: Option[String]
) {
  require(
    !classesDir.getFileName().toString().endsWith(".json"),
    s"the classes directory '$classesDir' ends with '.json', " +
      s"which Bloop will fail to load because it assumes it's a JSON file. " +
      s"To fix this problem, we need to change the implementation in Fastpass " +
      s"to not generate classes directories with the '.json' file extension."
  )
  def isGeneratedTarget: Boolean = name.startsWith(".pants.d")
  private val prefixedId = id.stripPrefix(".")
  def dependencyName: String =
    if (isGeneratedTarget) prefixedId
    else name

  def isModulizable: Boolean =
    isPantsModulizable &&
      pantsTargetType.isSupported
}
