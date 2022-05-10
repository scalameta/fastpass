package scala.meta.internal.fastpass.pantsbuild

import java.nio.file.Path

import scala.meta.internal.fastpass.generic.ExportOptions
import scala.meta.internal.fastpass.generic.OpenOptions
import scala.meta.internal.fastpass.generic.Project
import scala.meta.internal.fastpass.generic.SourcesMode
import scala.meta.internal.fastpass.pantsbuild.commands.StrictDepsMode
import scala.meta.io.AbsolutePath

import metaconfig.cli.CliApp
import org.eclipse.lsp4j.jsonrpc.CancelChecker

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
  val sources: SourcesMode =
    export.sources.toNonDefaultWithFallback(project.sources)
  val strictDeps: StrictDepsMode =
    export.strictDeps.toNonDefaultWithFallback(project.strictDeps)
  def bloopDir: Path = out.resolve(".bloop")
  def isMergeTargetsInSameDirectory: Boolean =
    export.mergeTargetsInSameDirectory
  def root = project.root
  def common = project.common
  def workspace = common.workspace
  def targets = project.targets
  def out = root.bspRoot.toNIO
  def pants: AbsolutePath = AbsolutePath(workspace.resolve("pants"))
}
