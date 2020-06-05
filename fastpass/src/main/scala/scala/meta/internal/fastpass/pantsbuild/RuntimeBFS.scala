package scala.meta.internal.fastpass.pantsbuild

import scala.collection.mutable

/**
 * Implementation of the Pants build graph traversal to compute the JVM runtime classpath.
 */
class RuntimeBFS(
    export: PantsExport,
    scope: SearchScope
) {
  private val dependencyCache = mutable.Map.empty[String, Iterable[PantsTarget]]
  private val isInProgress = new IdentityHashSet[String]

  def dependencies(target: PantsTarget): Iterable[PantsTarget] = {
    if (isInProgress.contains(target.name)) {
      throw new IllegalArgumentException(
        s"illegal cycle detected at target ${target.name}"
      )
    }
    def uncached(): Iterable[PantsTarget] = {
      isInProgress.add(target.name)
      val result = new mutable.LinkedHashSet[PantsTarget]()
      val deps = target.dependencies.map(export.targets)
      result ++= deps
      deps.foreach { dep =>
        val isContinue = scope match {
          case CompileScope => dep.scope.isCompile
          case RuntimeScope => dep.scope.isRuntime
        }
        if (isContinue) {
          result ++= dependencies(dep)
        }
      }
      isInProgress.remove(target.name)
      result
    }
    dependencyCache.getOrElseUpdate(target.name, uncached())
  }

}
