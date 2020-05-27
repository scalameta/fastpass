package scala.meta.internal.fastpass

object LogMessages {
  def pluralName(name: String, count: Int): String =
    s"${count} ${name}${plural(count)}"
  def plural(count: Int): String =
    if (count == 1) ""
    else "s"
}
