package scala.meta.internal.fastpass.pantsbuild.commands

sealed abstract class SourcesMode(override val toString: String)

object SourcesMode {
  case object On extends SourcesMode("on")
  case object Off extends SourcesMode("off")
  case object OnDemand extends SourcesMode("on-demand")

  def parse(s: String): SourcesMode = s match {
    case On.toString | "yes" | "true" | "enabled" => On
    case Off.toString | "no" | "false" | "disabled" => Off
    case OnDemand.toString => OnDemand
    case _ =>
      throw new RuntimeException(
        s"Invalid source mode $s. Use: 'on', 'off' or 'on-demand'"
      )
  }
}
