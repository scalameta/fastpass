package scala.meta.internal.fastpass.pantsbuild

/** Enum for whether to search for the compile-time or runtime classpath. */
sealed abstract class SearchScope
case object CompileScope extends SearchScope
case object RuntimeScope extends SearchScope
