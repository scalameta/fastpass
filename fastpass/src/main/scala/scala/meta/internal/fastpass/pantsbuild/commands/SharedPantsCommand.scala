package scala.meta.internal.fastpass.pantsbuild.commands

import scala.concurrent.ExecutionContext
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import scala.meta.internal.fastpass.MessageOnlyException
import scala.meta.internal.fastpass.Time
import scala.meta.internal.fastpass.Timer
import scala.meta.internal.fastpass.pantsbuild.BloopPants
import scala.meta.internal.fastpass.pantsbuild.Export
import scala.meta.internal.fastpass.pantsbuild.PantsExportResult

object SharedPantsCommand {
  def interpretExport(export: Export): Try[Option[PantsExportResult]] = {
    if (!export.pants.isFile) {
      Failure(
        MessageOnlyException(
          s"no Pants build detected, file '${export.pants}' does not exist. " +
            s"To fix this problem, change the working directory to the root of a Pants build."
        )
      )
    } else {
      val workspace = export.workspace
      val timer = new Timer(Time.system)
      if (export.open.intellij) {
        // there is no need to export bloop projects before starting intellij
        // as it will request buildTargets bsp endpoint that will call fastpass refresh
        Success(None)
      } else {
        BloopPants.bloopInstall(export)(ExecutionContext.global).map(Some(_))
      }
    }
  }
}
