package scala.meta.internal.fastpass.pantsbuild

import java.{util => ju}

class IdentityHashSet[T](
    underlying: ju.Set[T] =
      ju.Collections.newSetFromMap(new ju.IdentityHashMap[T, java.lang.Boolean])
) {
  def add(value: T): Boolean = underlying.add(value)
  def remove(value: T): Boolean = underlying.remove(value)
  def contains(value: T): Boolean = underlying.contains(value)
}
