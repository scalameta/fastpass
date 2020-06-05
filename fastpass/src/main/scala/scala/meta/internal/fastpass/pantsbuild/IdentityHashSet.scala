package scala.meta.internal.fastpass.pantsbuild

import java.{util => ju}

class IdentityHashSet[T](
    underlying: ju.Set[T] =
      ju.Collections.newSetFromMap(new ju.IdentityHashMap[T, java.lang.Boolean])
) {
  def add(value: T) = underlying.add(value)
  def remove(value: T) = underlying.remove(value)
  def contains(value: T) = underlying.contains(value)
}
