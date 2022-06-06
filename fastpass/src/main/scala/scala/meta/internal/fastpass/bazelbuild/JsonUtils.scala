package scala.meta.internal.fastpass.bazelbuild

import java.util.Base64
import java.util.LinkedHashMap

import com.google.protobuf.MessageLite
import ujson.Arr
import ujson.Obj
import ujson.Str
import ujson.Value

object JsonUtils {

  class ProtoIndex {
    private val index = new LinkedHashMap[MessageLite, Int]()

    def getOrUpdate(message: MessageLite): Int = {
      index.computeIfAbsent(message, _ => index.size)
    }

    def toJson: Value = {
      val arr = Arr()
      index.forEach {
        case (key, _) =>
          val js = protoToJson(key)
          arr.value.append(js)
      }
      arr
    }

    private def protoToJson(msg: MessageLite): Value = {
      val str = new String(Base64.getEncoder.encode(msg.toByteArray()))
      Str(str)
    }
  }

  def protoToJson(index: ProtoIndex, msg: MessageLite): Value = {
    index.getOrUpdate(msg)
  }

  def jsonToProto[T](index: IndexedSeq[Value], js: Value)(
      op: Array[Byte] => T
  ): T = {
    val item = index(js.num.toInt)
    val bytes = Base64.getDecoder().decode(item.str)
    op(bytes)
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

}
