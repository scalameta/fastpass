package scala.meta.internal.fastpass

import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import java.nio.file.Files
import java.nio.file.Path

import scala.annotation.switch

object FileUtils {
  def withTempFile[T](op: Path => T): T = {
    val file = Files.createTempFile("fastpass", null)
    try op(file)
    finally Files.deleteIfExists(file)
  }

  def copy(in: InputStream, out: OutputStream, bufSize: Int = 16384): Unit = {
    var read: Int = -1
    val buffer = new Array[Byte](bufSize)
    while ({ read = in.read(buffer); read } != -1) {
      out.write(buffer, 0, read)
    }
  }

  def sanitizeFileName(
      name: String,
      suffix: String = "",
      maxLength: Int = 255
  ): String = {
    val availableLength = maxLength - suffix.length
    name.replace("/", ".").replace(":", ".").take(availableLength) + suffix
  }

  def makeReadableFilename(target: String): String = {
    val out = new java.lang.StringBuilder(target.length())
    var i = 0
    while (i < target.length()) {
      val ch = (target.charAt(i): @switch) match {
        case '.' => '.'
        case '_' => '_'
        case ch =>
          if (Character.isAlphabetic(ch)) ch
          else if (Character.isDigit(ch)) ch
          else '-'
      }
      out.append(ch)
      i += 1
    }
    out.toString()
  }
  def makeJsonFilename(target: String): String = {
    makeReadableFilename(target) + ".json"
  }
  def makeJarFilename(target: String): String = {
    makeReadableFilename(target) + ".jar"
  }
  def makeClassesDirFilename(target: String): String = {
    // Prepend "z_" to separate it from the JSON files when listing the
    // `.bloop/` directory.
    "z_" + MD5.compute(target).take(12)
  }

  object NullOutputStream extends OutputStream {
    override def write(b: Int): Unit = ()
  }

  object NullPrintStream extends PrintStream(NullOutputStream)

}
