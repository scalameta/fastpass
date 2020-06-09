package scala.meta.internal.fastpass.pantsbuild

import scala.meta.io.AbsolutePath
import org.eclipse.lsp4j.jsonrpc.CancelChecker
import scala.meta.internal.fastpass.pantsbuild.commands.OpenOptions
import scala.meta.internal.fastpass.pantsbuild.commands.Project
import metaconfig.cli.CliApp
import scala.meta.internal.fastpass.pantsbuild.commands.ExportOptions

/**
 * The command-line argument parser for BloopPants.
 */
case class Export(
    project: Project,
    open: OpenOptions,
    app: CliApp,
    export: ExportOptions = ExportOptions.default,
    isCache: Boolean = false,
    isRegenerate: Boolean = false,
    token: CancelChecker = () => ()
) {
  val sources = export.sources.toNonDefaultWithFallback(project.sources)
  def bloopDir = out.resolve(".bloop")
  def isMergeTargetsInSameDirectory: Boolean =
    export.mergeTargetsInSameDirectory
  def root = project.root
  def common = project.common
  def workspace = common.workspace
  def targets = project.targets
  def out = root.bspRoot.toNIO
  def pants: AbsolutePath = AbsolutePath(workspace.resolve("pants"))
}
