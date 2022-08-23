package scala.meta.internal.fastpass.generic

import scala.collection.immutable.Nil

import scala.meta.internal.fastpass.pantsbuild.PantsConfiguration

import metaconfig.ConfDecoder
import metaconfig.ConfEncoder
import metaconfig.annotation._
import metaconfig.generic
import metaconfig.generic.Settings

case class CreateOptions(
    @ExtraName("remainingArgs")
    @Hidden()
    targets: List[String] = Nil,
    @Description(
      "The name of the generated project that appears in the IntelliJ projects view. " +
        "Should ideally be short, readable and easy to type. " +
        "Defaults to an auto-generated name based on the --targets option."
    )
    name: Option[String] = None,
    @Description(
      "Whether to use Bazel for project import. Inferred from target specs."
    )
    bazel: Boolean = true,
    @Description(
      "Whether to use Pants for project import. Inferred from target specs."
    )
    forcePants: Boolean = false,
    @Description(
      "Whether to ignore the remote export cache. Defaults to false."
    )
    ignoreCache: Boolean = false,
    @Description(
      "Whether to exit after updating the remote cache. Defaults to false."
    )
    stopAfterCache: Boolean = false,
    @Hidden() @Inline export: ExportOptions = ExportOptions.default,
    @Hidden() @Inline open: OpenOptions = OpenOptions.default,
    @Hidden() @Inline common: SharedOptions = SharedOptions.default
) {
  def actualName: String =
    name match {
      case Some(supplied) => PantsConfiguration.outputFilename(supplied)
      case None => PantsConfiguration.outputFilename(targets)
    }
}

object CreateOptions {
  val default: CreateOptions = CreateOptions()
  implicit lazy val surface: generic.Surface[CreateOptions] =
    generic.deriveSurface[CreateOptions]
  implicit lazy val encoder: ConfEncoder[CreateOptions] =
    generic.deriveEncoder[CreateOptions]
  implicit lazy val decoder: ConfDecoder[CreateOptions] =
    generic.deriveDecoder[CreateOptions](default)
  implicit lazy val settings: Settings[CreateOptions] = Settings[CreateOptions]
}
