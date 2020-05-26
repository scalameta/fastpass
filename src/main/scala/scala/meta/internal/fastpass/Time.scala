package scala.meta.internal.fastpass

/**
 * Wrapper around `System.nanoTime` and `System.currentTimeMillis`
 * to allow easier testing.
 */
trait Time {
  def nanos(): Long
  def currentMillis(): Long
}

object Time {
  object system extends Time {
    def nanos(): Long = System.nanoTime()
    def currentMillis(): Long = System.currentTimeMillis()
  }
}
