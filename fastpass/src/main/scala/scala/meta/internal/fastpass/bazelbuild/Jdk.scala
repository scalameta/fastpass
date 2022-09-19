package scala.meta.internal.fastpass.bazelbuild

import java.nio.file.Path
import java.nio.file.Paths

import scala.meta.internal.fastpass.bazelbuild.Build.Target

object Jdk {

  sealed abstract class Scope(val bazelAttr: String)
  case object Compile extends Scope("java_compile_toolchain")
  case object Runtime extends Scope("runtime_jdk")

  def javaHome(target: Target, scope: Scope): Path = {
    Bazel.getAttribute(target, scope.bazelAttr).map(_.getStringValue) match {
      case Some("//tools/jdks:twitter_jdk8_toolchain") => Jdk8
      case _ => Jdk11
    }
  }

  private val Jdk8 = Paths.get("/tmp/bazel_tools/jdk/8")
  private val Jdk11 = Paths.get("/tmp/bazel_tools/jdk/11")

  val defaultJdk: Path = Paths.get("/tmp/bazel_tools/jdk/11")

}
