package scala.meta.internal.fastpass.pantsbuild

import scala.collection.mutable

/**
 * Implementation of the Pants build graph traversal to compute the JVM runtime classpath.
 */
class RuntimeBFS(
    export: PantsExport,
    scope: SearchScope
) {

  /** A Mapping from depth to nodes at this depth */
  private type DepthMap = mutable.ArrayBuffer[mutable.ArrayBuffer[PantsTarget]]

  private val cache = mutable.Map.empty[String, DepthMap]
  private val isInProgress = new IdentityHashSet[String]

  def dependencies(target: PantsTarget): Iterable[PantsTarget] = {
    depthMap(target).flatten
  }

  def depthMap(target: PantsTarget): DepthMap = {
    def uncached(): DepthMap = {
      isInProgress.add(target.name)
      val seen = new IdentityHashSet[PantsTarget]
      val result = new mutable.ArrayBuffer[mutable.ArrayBuffer[PantsTarget]]()
      val deps = target.dependencies.map(export.targets)
      if (deps.nonEmpty) {
        result += mutable.ArrayBuffer.concat(deps.filter(seen.add))

        val matchingDeps = deps.filter(_.scope.is(scope))
        val depsDepthMaps = matchingDeps.map(depthMap)
        val maxDepth =
          if (depsDepthMaps.isEmpty) 0 else depsDepthMaps.map(_.size).max

        (0 until maxDepth).foreach { depth =>
          val level = new mutable.ArrayBuffer[PantsTarget]()
          depsDepthMaps.foreach { dp =>
            if (dp.size > depth) {
              level ++= dp(depth).filter(seen.add)
            }
          }
          if (level.nonEmpty) result += level
        }
      }

      isInProgress.remove(target.name)
      result
    }

    if (isInProgress.contains(target.name)) {
      throw new IllegalArgumentException(
        s"illegal cycle detected at target ${target.name}"
      )
    }

    cache.getOrElseUpdate(target.name, uncached())
  }
}
