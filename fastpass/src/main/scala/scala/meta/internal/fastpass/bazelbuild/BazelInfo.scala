package scala.meta.internal.fastpass.bazelbuild

import scala.util.Try

import scala.meta.io.AbsolutePath

case class BazelInfo(
    javaHome: AbsolutePath,
    workspace: AbsolutePath,
    outputBase: AbsolutePath,
    executionRoot: AbsolutePath,
    bazelBin: AbsolutePath
)

object BazelInfo {
  def fromOutput(info: String): Try[BazelInfo] = {
    val infoMap = info.linesIterator.map { line =>
      val key = line.takeWhile(_ != ':')
      val value = line.substring(key.length + 2)
      key -> value
    }.toMap

    Try {
      BazelInfo(
        AbsolutePath(infoMap("java-home")),
        AbsolutePath(infoMap("workspace")),
        AbsolutePath(infoMap("output_base")),
        AbsolutePath(infoMap("execution_root")),
        AbsolutePath(infoMap("bazel-bin"))
      )
    }
  }
}
