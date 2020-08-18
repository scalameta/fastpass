package scala.meta.internal.fastpass.pantsbuild

import java.{util => ju}

import scala.collection.mutable

import scala.meta.internal.fastpass.pantsbuild.commands.StrictDepsMode

/**
 * Implementation of the Pants build graph traversal to compute the JVM compile-time classpath.
 */
class CompileBFS(export: PantsExport, mode: StrictDepsMode) {

  /** A Mapping from depth to nodes at this depth */
  private type DepthMap = mutable.ArrayBuffer[mutable.ArrayBuffer[PantsTarget]]

  private val exportCache = mutable.Map.empty[String, DepthMap]
  private val runtime = new RuntimeBFS(export, CompileScope)
  private val isInProgress = new ju.HashSet[String]

  def dependencies(target: PantsTarget): Iterable[PantsTarget] = {
    depthMap(target, mode.plusDepth).flatten
  }

  /**
   * Compute a mapping of ordered dependencies of `target`. Each list in the result
   * represents a new transitive step.
   *
   * Knowing the depth of visited nodes is required to cache and re-use previous results.
   */
  private def depthMap(target: PantsTarget, depth: Int): DepthMap = {
    if (mode.isTransitive || !target.strictDeps) {
      // Use the same classpath as at runtime
      runtime.depthMap(target)
    } else {
      val seen = new IdentityHashSet[PantsTarget]
      val result = new mutable.ArrayBuffer[mutable.ArrayBuffer[PantsTarget]]
      val deps = target.dependencies.map(export.targets)
      if (depth >= 0 && deps.nonEmpty) {
        result += mutable.ArrayBuffer.concat(deps.filter(seen.add))

        val newDepth =
          if (target.pantsTargetType.isTarget) depth
          else depth - 1

        val depsDepthMaps = deps.map(depthMap(_, newDepth))
        val exportsDepthMaps = deps.map(exportDepthMap)

        val maxDepth = Math.max(
          depsDepthMaps.map(_.size).max,
          exportsDepthMaps.map(_.size).max
        )

        (0 until maxDepth).foreach { currentDepth =>
          val level = new mutable.ArrayBuffer[PantsTarget]()

          // unlike exports, dependencies are followed only until `depth`.
          if (currentDepth < depth) {
            depsDepthMaps.foreach { depthMap =>
              if (depthMap.size > currentDepth) {
                level ++= depthMap(currentDepth).filter(seen.add)
              }
            }
          }

          exportsDepthMaps.foreach { depthMap =>
            if (depthMap.size > currentDepth) {
              level ++= depthMap(currentDepth).filter(seen.add)
            }
          }

          if (level.nonEmpty) result += level
        }
      }
      result
    }
  }

  private def exportDepthMap(target: PantsTarget): DepthMap = {
    def uncached(): DepthMap = {
      val result = new DepthMap
      val seen = new IdentityHashSet[PantsTarget]
      val exports = exportedTargets(target).filter(seen.add)
      if (exports.nonEmpty) {
        result += mutable.ArrayBuffer.concat(exports)
        val exportDepthMaps = exports.map(exportDepthMap)
        val maxDepth = exportDepthMaps.map(_.size).max
        (0 until maxDepth).foreach { depth =>
          val level = new mutable.ArrayBuffer[PantsTarget]()
          exportDepthMaps.foreach { dp =>
            if (dp.size > depth) {
              val toAdd = dp(depth).filter(seen.add)
              level ++= toAdd
            }
          }
          result += level
        }
      }
      result
    }
    exportCache.getOrElseUpdate(target.name, uncached())
  }

  private def exportedTargets(target: PantsTarget): Iterable[PantsTarget] = {
    if (!target.scope.isCompile) Nil
    else {
      if (
        target.pantsTargetType.isTarget || target.pantsTargetType.isJarLibrary
      ) {
        // NOTE(olafur): it seems like `target` and `jar_library` types
        // export their dependencies even if they have an empty `exports`
        // field. This should probably be reflected in the output of
        // `export-fastpass` but for now it's OK to do this logic on the
        // fastpass-side instead.
        target.dependencies.map(export.targets)
      } else {
        for {
          dependencyName <- target.dependencies
          if target.exports.contains(dependencyName) ||
            // NOTE(olafur): all synthetic dependencies are automatically
            // exported if they are derived from this target. We don't check
            // that this synthetic target is derived from this target, which
            // might cause subtle bugs that we may need to fix in the
            // future.
            export.targets(dependencyName).isSynthetic
        } yield export.targets(dependencyName)
      }
    }
  }
}
