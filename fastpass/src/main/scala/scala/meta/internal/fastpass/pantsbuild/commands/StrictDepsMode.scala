package scala.meta.internal.fastpass.pantsbuild.commands

import metaconfig.Conf
import metaconfig.Configured
import scala.meta.internal.fastpass.pantsbuild.commands.StrictDepsMode.PlusOne

sealed abstract class StrictDepsMode(val name: String) {
  def isStrict = this == StrictDepsMode.Strict
  def isPlusOne = this == StrictDepsMode.PlusOne
  def isTransitive = this == StrictDepsMode.Transitive
  def isDefault = this == StrictDepsMode.Default
  def toNonDefaultWithFallback(fallback: StrictDepsMode): StrictDepsMode =
    if (isDefault) fallback.toNonDefault
    else this
  def toNonDefault: StrictDepsMode =
    if (isDefault) StrictDepsMode.PlusOne
    else this
  override def toString(): String = name
}

object StrictDepsMode {
  case object Strict extends StrictDepsMode("strict")
  case object PlusOne extends StrictDepsMode("plus-one")
  case object Default extends StrictDepsMode("default")
  case object Transitive extends StrictDepsMode("transitive")
  implicit lazy val encoder: metaconfig.ConfEncoder[StrictDepsMode] =
    metaconfig.ConfEncoder.StringEncoder.contramap[StrictDepsMode](_.toString)
  implicit lazy val decoder: metaconfig.ConfDecoder[StrictDepsMode] =
    metaconfig.ConfDecoder.instanceExpect(s"$Strict|$PlusOne|$Transitive") {
      case Conf.Str(Strict.name) =>
        Configured.ok(Strict)
      case Conf.Str(PlusOne.name) =>
        Configured.ok(PlusOne)
      case Conf.Str(Transitive.name) =>
        Configured.ok(Transitive)
      case other =>
        Configured.error(
          s"Invalid --sources value '$other'. To fix this problem, use 'on', 'off' or 'on-demand'"
        )
    }
}
