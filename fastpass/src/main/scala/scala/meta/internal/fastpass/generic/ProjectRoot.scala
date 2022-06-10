package scala.meta.internal.fastpass.generic

import scala.meta.io.AbsolutePath

case class ProjectRoot(
    bspRoot: AbsolutePath
) {
  val bspJson: AbsolutePath = bspRoot.resolve(".bsp").resolve("bloop.json")
  val pantsLibrariesJson: AbsolutePath =
    bspRoot.resolve(".pants").resolve("libraries.json")
  val bloopRoot: AbsolutePath = bspRoot.resolve(".bloop")
  val venvRoot: AbsolutePath = bspRoot.resolve(".venv")
}
