package scala.meta.internal.fastpass.pantsbuild

import ujson.Value
import ujson.Obj
import ujson.Str

final case class PantsScope(scope: String) {
  private val isDefault =
    scope.contains("default") ||
      scope.contains("forced")
  val isCompile =
    isDefault ||
      scope.contains("compile")
  val isRuntime =
    isDefault ||
      scope.contains("runtime") ||
      scope.contains("test")
}

object PantsScope {
  val default = PantsScope("default")
  def fromJson(json: Obj): PantsScope = {
    json.value.get(PantsKeys.scope) match {
      case Some(Str(value)) => PantsScope(value)
      case _ => default
    }
  }
}
