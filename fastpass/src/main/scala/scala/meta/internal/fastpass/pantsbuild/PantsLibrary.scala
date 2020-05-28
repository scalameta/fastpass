package scala.meta.internal.fastpass.pantsbuild

import java.nio.file.Path

case class PantsLibrary(
    name: String,
    module: String,
    values: collection.Map[String, Path]
) {
  def default: Option[Path] = values.get("default")
  def sources: Option[Path] = values.get("sources")
  def nonSources: Iterator[Path] = {
    values.iterator.collect {
      case (key, path) if key != "sources" =>
        path
    }
  }
}
