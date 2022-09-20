package scala.meta.internal.fastpass.pantsbuild

import java.nio.file.Path

import scala.meta.io.AbsolutePath

import bloop.config.{Config => C}
import ujson.Arr
import ujson.Obj
import ujson.Str
import ujson.Value

case class PantsGlobs(
    include: List[String],
    exclude: List[String]
) {
  def isEmpty: Boolean = include.isEmpty

  def bloopConfig(workspace: Path, baseDirectory: Path): C.SourcesGlobs = {
    // Ensure the globs don't look at the parent directory (../) by translating
    // (base = foo/bar, includes = [../hello/*.scala, *.java]) into
    // (base = foo, includes = [hello/*.scala, bar/*.java])
    (
      include.partition(_.startsWith("../")),
      exclude.partition(_.startsWith("../"))
    ) match {
      case ((Nil, _), (Nil, _)) =>
        noParentDirectoryBloopConfig(workspace, baseDirectory)
      case (
            (inParentIncludes, otherIncludes),
            (inParentExcludes, otherExcludes)
          ) =>
        val currentDir = baseDirectory.getFileName.toString
        val newInParentIncludes = inParentIncludes.map(_.stripPrefix("../"))
        val newOtherIncludes = otherIncludes.map(currentDir + "/" + _)
        val newInParentExcludes = inParentExcludes.map(_.stripPrefix("../"))
        val newOtherExcludes = otherExcludes.map(currentDir + "/" + _)

        copy(
          include = newInParentIncludes ++ newOtherIncludes,
          exclude = newInParentExcludes ++ newOtherExcludes
        ).bloopConfig(workspace, baseDirectory.getParent)
    }
  }

  def toJson(): Value = {
    val newJson = Obj()
    newJson("globs") = include
    newJson("exclude") = exclude.map(ex => Obj("globs" -> List(ex)))
    newJson
  }

  private def noParentDirectoryBloopConfig(
      workspace: Path,
      baseDirectory: Path
  ): C.SourcesGlobs = {
    val prefix = AbsolutePath(baseDirectory)
      .toRelative(AbsolutePath(workspace))
      .toURI(true)
      .toString()
    def toNioPathMatcher(pattern: String): String = {
      // NOTE(olafur) Pants globs interpret "**/*.scala" as "zero or more
      // directories" while Bloop uses `java.nio.file.PathMatcher`, which
      // interprets it as "one or more directories".
      pattern.replace("**/*", "**")
    }
    def relativizeGlob(glob: String): String = {
      val pattern = glob.stripPrefix(prefix) match {
        case pat if pat.endsWith("/*") && !pat.endsWith("**/*") =>
          // NOTE(martin) Pants globs ending in `/*` will match everything in that
          // directory. The equivalent `java.nio.file.PathMatcher` is `/**`.
          toNioPathMatcher(pat.substring(0, pat.length - 2)) + "/**"
        case pat =>
          toNioPathMatcher(pat)
      }
      s"glob:$pattern"
    }
    val includeGlobs = include.map(relativizeGlob)
    val excludeGlobs = exclude.map(relativizeGlob)
    val walkDepth = PantsGlobs.walkDepth(includeGlobs)
    C.SourcesGlobs(
      baseDirectory,
      walkDepth = walkDepth,
      includes = includeGlobs,
      excludes = excludeGlobs
    )
  }
}

object PantsGlobs {
  val empty: PantsGlobs = PantsGlobs(Nil, Nil)
  def fromJson(target: Value): PantsGlobs = {
    target.obj.get("globs") match {
      case Some(obj: Obj) =>
        val include = globsFromObject(obj)
        val exclude = obj.value.get("exclude") match {
          case Some(arr: Arr) =>
            arr.value.iterator.flatMap(globsFromObject).toList
          case _ =>
            Nil
        }
        PantsGlobs(include, exclude)
      case _ =>
        PantsGlobs(Nil, Nil)
    }
  }

  private def globsFromObject(value: Value): List[String] =
    value match {
      case Obj(obj) =>
        obj.get("globs") match {
          case Some(arr: Arr) =>
            arr.value.iterator.collect {
              case Str(glob) => glob
            }.toList
          case _ =>
            Nil
        }
      case _ => Nil
    }

  private def walkDepth(globs: List[String]): Option[Int] = {
    if (globs.isEmpty) Some(0)
    else {
      val depth = globs.map { glob =>
        if (glob.contains("**")) Int.MaxValue
        else glob.count(_ == '/') + 1
      }.max
      if (depth == Int.MaxValue) None
      else Some(depth)
    }
  }
}
