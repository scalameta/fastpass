package scala.meta.internal.fastpass.bazelbuild
import ujson.Obj
import ujson.Value

final case class SourcesInfo(
    include: List[String],
    exclude: List[String],
    javaSources: List[String]
) {
  def toJson: Value = {
    val newJson = Obj()
    newJson("include") = include
    newJson("exclude") = exclude
    newJson("javaSources") = javaSources
    newJson
  }
}

object SourcesInfo {

  def fromJson(value: Value): SourcesInfo = {
    SourcesInfo(
      getStringList(value, "include"),
      getStringList(value, "exclude"),
      getStringList(value, "javaSources")
    )
  }

  private def getStringList(value: Value, key: String): List[String] =
    value.obj.get(key).map(_.arr.map(_.str).toList).getOrElse(Nil)
}
