package scala.meta.internal.fastpass.bazelbuild

import java.util.Base64

import com.google.protobuf.MessageLite
import ujson.Obj
import ujson.Str
import ujson.Value

object JsonUtils {
  def protoToJson(msg: MessageLite): Str = {
    val str = new String(Base64.getEncoder.encode(msg.toByteArray()))
    Str(str)
  }

  def mapToJson[K, V](obj: Map[K, V])(
      keyKey: String,
      keyOp: K => Value,
      valueKey: String,
      valueOp: V => Value
  ): List[Obj] = {
    obj.toList.map {
      case (key, value) =>
        val newJson = Obj()
        newJson(keyKey) = keyOp(key)
        newJson(valueKey) = valueOp(value)
        newJson
    }
  }

  def mapFromJson[K, V](
      js: Value,
      keyKey: String,
      keyOp: Value => K,
      valueKey: String,
      valueOp: Value => V
  ): Map[K, V] = {
    js.arr.toList.map { entry =>
      val key = keyOp(entry(keyKey))
      val value = valueOp(entry(valueKey))
      key -> value
    }.toMap
  }

  def jsonToProto[T](js: Value)(op: Array[Byte] => T): T = {
    val bytes = Base64.getDecoder().decode(js.str)
    op(bytes)
  }

}
