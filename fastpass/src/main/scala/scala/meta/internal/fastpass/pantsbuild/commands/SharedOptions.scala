package scala.meta.internal.fastpass.pantsbuild.commands

import java.nio.file.Path
import java.nio.file.Paths

import scala.meta.internal.fastpass.pantsbuild.Codecs._
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath

import metaconfig.ConfDecoder
import metaconfig.ConfEncoder
import metaconfig.annotation._
import metaconfig.generic
import metaconfig.generic.Settings

case class SharedOptions(
    @Description("The root directory of the Pants build.")
    workspace: Path = PathIO.workingDirectory.toNIO,
    @Description(
      "The path to the `pants` executable. " +
        "Defaults to the `pants` executable in the workspace directory."
    )
    pants: Option[Path] = None
) {
  def bloopDirectory: Path = workspace.resolve(".bloop")
  def pantsBinary: Path = pants.getOrElse(workspace.resolve("pants"))
  val home: AbsolutePath = AbsolutePath {
    Option(System.getenv("FASTPASS_HOME")) match {
      case Some(value) => Paths.get(value)
      case None => workspace.resolveSibling("bsp-projects")
    }
  }
}

object SharedOptions {
  val default: SharedOptions = SharedOptions()
  implicit lazy val surface: generic.Surface[SharedOptions] =
    generic.deriveSurface[SharedOptions]
  implicit lazy val encoder: ConfEncoder[SharedOptions] =
    generic.deriveEncoder[SharedOptions]
  implicit lazy val decoder: ConfDecoder[SharedOptions] =
    generic.deriveDecoder[SharedOptions](default)
  implicit lazy val settings: Settings[SharedOptions] = Settings[SharedOptions]
}
