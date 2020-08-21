package scala.meta.internal.fastpass.pantsbuild.commands

import metaconfig.ConfDecoder
import metaconfig.ConfEncoder
import metaconfig.annotation._
import metaconfig.generic
import metaconfig.generic.Settings

case class CurrentOptions(
    @Inline common: SharedOptions = SharedOptions()
)
object CurrentOptions {
  val default: CurrentOptions = CurrentOptions()
  implicit lazy val surface: generic.Surface[CurrentOptions] =
    generic.deriveSurface[CurrentOptions]
  implicit lazy val encoder: ConfEncoder[CurrentOptions] =
    generic.deriveEncoder[CurrentOptions]
  implicit lazy val decoder: ConfDecoder[CurrentOptions] =
    generic.deriveDecoder[CurrentOptions](default)
  implicit lazy val settings: Settings[CurrentOptions] =
    Settings[CurrentOptions]
}
