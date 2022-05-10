package scala.meta.internal.fastpass.generic

sealed abstract class ImportMode(val name: String)
object ImportMode {
  case object Pants extends ImportMode("pants")
}
