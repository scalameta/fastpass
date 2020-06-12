package scala.meta.internal.fastpass.pantsbuild.commands

import metaconfig.generic
import metaconfig.annotation._
import metaconfig.ConfCodec
import java.nio.file.Path

case class ExportOptions(
    @Description(
      "Mode to handle dependency sources." +
        "Use 'off' to disable downloading sources for 3rd party libraries. " +
        "Use 'on' to download and link te sources automatically. " +
        "Use 'on-demand' to download sources and allow linking them later."
    )
    sources: SourcesMode = SourcesMode.Default,
    @Description(
      "Mode to handle strict_deps. " +
        "Use 'strict' to accurately reproduce strict_deps, disabled by default because it may produce undesirable behavior in the IDE and produce unexpected compile errors in some case. " +
        "Use 'plus-one' to include direct dependencies as well as their direct dependencies, enabled by default because it strikes good balance between correctness and convenience in the IDE. " +
        "Use 'transitive' to ignore strict_deps and include all transitive dependencies on the compile classpath."
    )
    strictDeps: StrictDepsMode = StrictDepsMode.Default,
    @Description(
      "The path to the coursier binary." +
        "If unspecified, coursier will be downloaded automatically."
    )
    coursierBinary: Option[Path] = None,
    @Hidden()
    @Description("When enabled, Fastpass will not exit the Bloop server.")
    noBloopExit: Boolean = false,
    @Hidden()
    mergeTargetsInSameDirectory: Boolean = false
) {
  def canBloopExit: Boolean = !noBloopExit
}

object ExportOptions {
  val default: ExportOptions = ExportOptions()
  implicit lazy val surface: generic.Surface[ExportOptions] =
    generic.deriveSurface[ExportOptions]
  implicit lazy val codec: ConfCodec[ExportOptions] =
    generic.deriveCodec[ExportOptions](default)
}
