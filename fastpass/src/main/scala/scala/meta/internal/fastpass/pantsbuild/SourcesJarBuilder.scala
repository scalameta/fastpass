package scala.meta.internal.fastpass.pantsbuild

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.FileSystems
import java.nio.file.FileVisitOption
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.PathMatcher
import java.nio.file.SimpleFileVisitor
import java.nio.file.StandardOpenOption
import java.nio.file.attribute.BasicFileAttributes

import scala.util.control.NonFatal

import scala.meta.internal.fastpass.FastpassEnrichments._
import scala.meta.io.AbsolutePath
import scala.meta.io.RelativePath

import bloop.config.Config.SourcesGlobs

/** Helper class to generate `*-sources.jar` files. */
class SourcesJarBuilder(export: PantsExport, root: Path) {
  def writeSourceRoot(sourceRoot: RelativePath): Unit =
    try {
      val out =
        root.resolve("META-INF").resolve("fastpass").resolve("source-root")
      Files.createDirectories(out.getParent)
      Files.write(
        out,
        sourceRoot.toString().getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
    } catch {
      case NonFatal(_) =>
    }

  private def write(source: AbsolutePath, path: RelativePath): Unit = {
    val out = root.resolve(path.toString())
    val text = source.readText
    try {
      if (out.getParent() != root) {
        Files.createDirectories(out.getParent())
      }
      Files.write(
        out,
        text.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.CREATE
      )
    } catch {
      case NonFatal(_) =>
    }
  }

  def expandDirectory(
      dir: AbsolutePath,
      relativizeBy: AbsolutePath
  ): Unit = {
    Files.walkFileTree(
      dir.toNIO,
      java.util.EnumSet.of(FileVisitOption.FOLLOW_LINKS),
      Int.MaxValue,
      new SimpleFileVisitor[Path] {
        override def visitFileFailed(
            file: Path,
            exc: IOException
        ): FileVisitResult = {
          FileVisitResult.CONTINUE
        }
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          write(
            AbsolutePath(file),
            AbsolutePath(file).toRelative(relativizeBy)
          )
          FileVisitResult.CONTINUE
        }
      }
    )
  }

  def expandGlob(glob: SourcesGlobs, relativizeBy: AbsolutePath): Unit = {
    val fs = FileSystems.getDefault()
    val includes = glob.includes.map(fs.getPathMatcher)
    val excludes = glob.excludes.map(fs.getPathMatcher)
    val walkDepth = glob.walkDepth.getOrElse(Int.MaxValue)
    def matches(path: Path): Boolean = {
      val relativePath =
        AbsolutePath(path).toRelative(AbsolutePath(glob.directory))
      def matchesList(lst: List[PathMatcher]): Boolean =
        lst match {
          case Nil => false
          case head :: tail =>
            if (head.matches(relativePath.toNIO)) true
            else matchesList(tail)
        }
      matchesList(includes) && !matchesList(excludes)
    }
    Files.walkFileTree(
      glob.directory,
      java.util.EnumSet.of(FileVisitOption.FOLLOW_LINKS),
      glob.walkDepth.getOrElse(Int.MaxValue),
      new SimpleFileVisitor[Path] {
        override def visitFileFailed(
            file: Path,
            exc: IOException
        ): FileVisitResult = {
          FileVisitResult.CONTINUE
        }
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes
        ): FileVisitResult = {
          if (matches(file)) {
            write(
              AbsolutePath(file),
              AbsolutePath(file).toRelative(relativizeBy)
            )
          }
          FileVisitResult.CONTINUE
        }
      }
    )
  }
}
