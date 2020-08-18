package scala.meta.internal.fastpass.pantsbuild

import ujson.Obj
import ujson.Str

final case class PantsScope(scope: String) {
  private val isDefault =
    scope.contains("default") ||
      scope.contains("forced")
  val isCompile: Boolean =
    isDefault ||
      scope.contains("compile")
  val isRuntime: Boolean =
    isDefault ||
      scope.contains("runtime") ||
      scope.contains("test")
  def is(scope: SearchScope): Boolean =
    scope match {
      case CompileScope => isCompile
      case RuntimeScope => isRuntime
    }
}

object PantsScope {
  val default: PantsScope = PantsScope("default")
  def fromJson(json: Obj): PantsScope = {
    json.value.get(PantsKeys.scope) match {
      case Some(Str(value)) => PantsScope(value)
      case _ => default
    }
  }
}
