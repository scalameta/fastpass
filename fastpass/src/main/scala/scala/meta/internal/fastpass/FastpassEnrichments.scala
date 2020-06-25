package scala.meta.internal.fastpass

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.nio.file.StandardOpenOption

import scala.collection.convert.DecorateAsJava
import scala.collection.convert.DecorateAsScala
import scala.collection.mutable
import scala.util.control.NonFatal

import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath

import geny.Generator

object FastpassEnrichments extends DecorateAsScala with DecorateAsJava {

  implicit class XtensionStream[A](stream: java.util.stream.Stream[A]) {
    def asScala: Generator[A] = {
      Generator.selfClosing((stream.iterator.asScala, () => stream.close()))
    }
  }

  implicit class XtensionIterableOps[T](lst: Iterable[T]) {
    def distinctBy[B](fn: T => B): List[T] = {
      new XtensionIteratorOps(lst.iterator).distinctBy(fn)
    }
  }

  implicit class XtensionIteratorOps[T](lst: Iterator[T]) {
    def distinctBy[B](fn: T => B): List[T] = {
      val isVisited = mutable.Set.empty[B]
      val buf = mutable.ListBuffer.empty[T]
      lst.foreach { elem =>
        val hash = fn(elem)
        if (!isVisited(hash)) {
          isVisited += hash
          buf += elem
        }
      }
      buf.result()
    }
  }

  implicit class XtensionAbsolutePath(path: AbsolutePath) {
    def parent: AbsolutePath = {
      AbsolutePath(path.toNIO.getParent)
    }

    def createDirectories(): AbsolutePath =
      AbsolutePath(Files.createDirectories(path.dealias.toNIO))

    // Using [[Files.isSymbolicLink]] is not enough.
    // It will be false when one of the parents is a symlink (e.g. /dir/link/file.txt)
    def dealias: AbsolutePath = {
      if (exists) { // cannot dealias non-existing path
        AbsolutePath(path.toNIO.toRealPath())
      } else {
        path
      }
    }

    def readText: String = {
      FileIO.slurp(path, StandardCharsets.UTF_8)
    }

    def writeText(text: String): Unit = {
      path.parent.createDirectories()
      val tmp = Files.createTempFile("metals", path.filename)
      // Write contents first to a temporary file and then try to
      // atomically move the file to the destination. The atomic move
      // reduces the risk that another tool will concurrently read the
      // file contents during a half-complete file write.
      Files.write(
        tmp,
        text.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.TRUNCATE_EXISTING
      )
      try {
        Files.move(
          tmp,
          path.toNIO,
          StandardCopyOption.REPLACE_EXISTING,
          StandardCopyOption.ATOMIC_MOVE
        )
      } catch {
        case NonFatal(_) =>
          Files.move(tmp, path.toNIO, StandardCopyOption.REPLACE_EXISTING)
      }
    }

    def exists: Boolean = {
      Files.exists(path.toNIO)
    }

    def filename: String = path.toNIO.getFileName.toString

    def list: Generator[AbsolutePath] = {
      if (path.isDirectory) Files.list(path.toNIO).asScala.map(AbsolutePath(_))
      else Generator()
    }

    def listRecursive: Generator[AbsolutePath] = {
      if (path.isDirectory) Files.walk(path.toNIO).asScala.map(AbsolutePath(_))
      else if (path.isFile) Generator(path)
      else Generator()
    }

    def isScala: Boolean = {
      filename.endsWith(".scala")
    }
  }
}
