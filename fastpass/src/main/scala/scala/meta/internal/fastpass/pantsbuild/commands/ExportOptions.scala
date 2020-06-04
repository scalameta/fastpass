package scala.meta.internal.fastpass.pantsbuild.commands

import metaconfig.generic
import metaconfig.annotation._
import metaconfig.ConfCodec
import java.nio.file.Path

case class ExportOptions(
    @Description(
      "Control sources. Use 'off' to disable downloading sources for 3rd party libraries. " +
        "Use 'on' to download and link te sources automatically. " +
        "Use 'on-demand' to download sources and allow linking them later."
    )
    sources: Option[String] = None,
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

  def disableSources: Boolean = sourcesMode == SourcesMode.Off
  def enableSources: Boolean = sourcesMode == SourcesMode.On
  def onDemandSources: Boolean = sourcesMode == SourcesMode.OnDemand

  lazy val sourcesMode: SourcesMode =
    sources.map(SourcesMode.parse).getOrElse(SourcesMode.On)
}

object ExportOptions {
  val default: ExportOptions = ExportOptions()
  implicit lazy val surface: generic.Surface[ExportOptions] =
    generic.deriveSurface[ExportOptions]
  implicit lazy val codec: ConfCodec[ExportOptions] =
    generic.deriveCodec[ExportOptions](default)
}
