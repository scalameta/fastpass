package scala.meta.internal.fastpass.pantsbuild.commands

import metaconfig.Conf
import metaconfig.Configured

sealed abstract class SourcesMode(override val toString: String) {
  def isOff: Boolean = this == SourcesMode.Off
  def isOn: Boolean = this == SourcesMode.On
  def isOnDemand: Boolean = this == SourcesMode.OnDemand
  def isDefault: Boolean = this == SourcesMode.Default
  def toNonDefault: SourcesMode =
    if (isDefault) SourcesMode.On
    else this
  def toNonDefaultWithFallback(fallback: SourcesMode): SourcesMode =
    if (isDefault) fallback.toNonDefault
    else toNonDefault
}

object SourcesMode {
  case object On extends SourcesMode("on")
  case object Off extends SourcesMode("off")
  case object Default extends SourcesMode("default")
  case object OnDemand extends SourcesMode("on-demand")

  implicit lazy val encoder: metaconfig.ConfEncoder[SourcesMode] =
    metaconfig.ConfEncoder.StringEncoder.contramap[SourcesMode](_.toString)
  implicit lazy val decoder: metaconfig.ConfDecoder[SourcesMode] =
    metaconfig.ConfDecoder.instanceExpect(s"$On|$Off|$OnDemand") {
      case Conf.Str(On.toString | "yes" | "true" | "enabled") |
          Conf.Bool(true) =>
        Configured.ok(On)
      case Conf.Str(Off.toString | "no" | "false" | "disabled") |
          Conf.Bool(false) =>
        Configured.ok(Off)
      case Conf.Str(OnDemand.toString) => Configured.ok(OnDemand)
      case Conf.Str(Default.toString) => Configured.ok(Default)
      case other =>
        Configured.error(
          s"Invalid --sources value '$other'. To fix this problem, use 'on', 'off' or 'on-demand'"
        )
    }
}
