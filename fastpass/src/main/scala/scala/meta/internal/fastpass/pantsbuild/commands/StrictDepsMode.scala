package scala.meta.internal.fastpass.pantsbuild.commands

import scala.util.matching.Regex

import metaconfig.Conf
import metaconfig.Configured

sealed abstract class StrictDepsMode(val name: String) {
  def isStrict: Boolean = this == StrictDepsMode.Strict
  def plusDepth: Int =
    this match {
      case StrictDepsMode.Strict => 0
      case StrictDepsMode.Plus(n) => n
      case StrictDepsMode.Transitive => Int.MaxValue
      case other => throw new IllegalArgumentException(s"$this has no depth")
    }
  def isTransitive: Boolean = this == StrictDepsMode.Transitive
  def isDefault: Boolean = this == StrictDepsMode.Default
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
  case class Plus(n: Int) extends StrictDepsMode(s"plus-$n")
  val PlusOne: Plus = Plus(1)
  case object Default extends StrictDepsMode("default")
  case object Transitive extends StrictDepsMode("transitive")
  implicit lazy val encoder: metaconfig.ConfEncoder[StrictDepsMode] =
    metaconfig.ConfEncoder.StringEncoder.contramap[StrictDepsMode](_.toString)
  val PlusRegex: Regex = "plus-(\\d+)".r
  implicit lazy val decoder: metaconfig.ConfDecoder[StrictDepsMode] =
    metaconfig.ConfDecoder.instanceExpect(s"$Strict|${Plus(1)}|$Transitive") {
      case Conf.Str(Strict.name) =>
        Configured.ok(Strict)
      case Conf.Str(PlusRegex(number)) =>
        Configured.ok(Plus(number.toInt))
      case Conf.Str(Transitive.name) =>
        Configured.ok(Transitive)
      case other =>
        Configured.error(
          s"Invalid --strict-deps value '$other'. " +
            s"To fix this problem, use 'strict', 'plus-1', 'plus-2' or 'transitive'"
        )
    }
}
