package scala.meta.internal.fastpass.bazelbuild

import java.io.InputStream
import java.io.OutputStream
import java.net.HttpURLConnection
import java.net.URI
import java.net.URL
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import scala.meta.internal.fastpass.MessageOnlyException
import scala.meta.internal.fastpass.generic.Project

import org.tukaani.xz.LZMA2Options
import org.tukaani.xz.XZInputStream
import org.tukaani.xz.XZOutputStream

sealed trait RemoteCache {
  def getFromCache[T](filename: String)(op: InputStream => T): Try[T]
  def writeToCache(filename: String)(op: OutputStream => Unit): Try[Boolean]
}

class NoRemoteCache(reason: Throwable) extends RemoteCache {
  override def getFromCache[T](filename: String)(op: InputStream => T): Try[T] =
    Failure(reason)
  override def writeToCache(
      filename: String
  )(op: OutputStream => Unit): Try[Boolean] =
    Failure(reason)
}

/**
 * A remote cache implementation that reads from and writes to the local disk. It can be used to
 * test serialization and deserialization.
 *
 * Artifacts will be stored in `$baseDirectory/$cacheKey/$filename`.
 */
private class FileRemoteCache(
    baseDirectory: Path,
    cacheKey: String
) extends RemoteCache {

  private val cacheRoot = baseDirectory.resolve(cacheKey)
  Files.createDirectories(cacheRoot)

  override def getFromCache[T](
      filename: String
  )(op: InputStream => T): Try[T] = {
    val path = cacheRoot.resolve(filename)
    Try {
      val stream = Files.newInputStream(path)
      try op(stream)
      finally stream.close()
    }
  }

  override def writeToCache(
      filename: String
  )(op: OutputStream => Unit): Try[Boolean] = {
    val path = cacheRoot.resolve(filename)
    Try {
      val stream = Files.newOutputStream(path)
      try { op(stream); true }
      finally stream.close()
    }
  }

}

private class HttpRemoteCache(
    cacheConfig: CacheConfig,
    cacheKey: String
) extends RemoteCache {
  def getFromCache[T](
      filename: String
  )(op: InputStream => T): Try[T] = {
    val url = artifactUrl(filename)
    Try {
      val connection = url.openConnection()
      val stream = new XZInputStream(connection.getInputStream())
      try op(stream)
      finally stream.close()
    }
  }

  def writeToCache(filename: String)(op: OutputStream => Unit): Try[Boolean] = {
    if (!cacheConfig.enableUpload) Success(false)
    else {
      val url = artifactUrl(filename)
      Try {
        url.openConnection() match {
          case connection: HttpURLConnection =>
            connection.setDoOutput(true)
            connection.setRequestMethod("PUT")
            cacheConfig.credentials.map(_.encoded).foreach {
              connection.setRequestProperty("Authorization", _)
            }
            val stream = new XZOutputStream(
              connection.getOutputStream(),
              // Export may be >32MB, so use large dictionary size.
              // See https://tukaani.org/xz/xz-javadoc/org/tukaani/xz/LZMA2Options.html#setPreset(int)
              new LZMA2Options(9)
            )
            try {
              op(stream)
              stream.flush()
            } finally {
              stream.close()
              connection.disconnect()
            }

            val responseCode = connection.getResponseCode()
            if (responseCode != HttpURLConnection.HTTP_OK) {
              val responseMessage = connection.getResponseMessage()
              throw new MessageOnlyException(
                s"Write to cache failed: $responseCode $responseMessage"
              )
            } else {
              true
            }

          case _ =>
            throw new MessageOnlyException(
              "Caching is only supported over HTTP(S)"
            )
        }
      }
    }
  }

  private def artifactUrl(filename: String): URL =
    cacheConfig.uri
      .resolve(filename + ".xz/")
      .resolve(cacheKey)
      .toURL()
}

private case class CacheConfig(
    uri: URI,
    enableUpload: Boolean,
    credentials: Option[Credentials]
)

object RemoteCache {
  def configure(
      bazel: Bazel,
      project: Project,
      cacheFormatVersion: String
  ): RemoteCache = {
    val cache = for {
      config <- readConfig(project.common.workspace)
      key <- computeCacheKey(bazel, project, cacheFormatVersion)
    } yield new HttpRemoteCache(config, key)

    cache match {
      case Failure(ex) => new NoRemoteCache(ex)
      case Success(result) => result
    }
  }

  private def readConfig(workspace: Path): Try[CacheConfig] = {
    val configFile = workspace.resolve(".fastpass.json")
    Try {
      val js = ujson.read(configFile)
      val url = URI.create(js("cache-url").str)
      val credentials =
        js.obj
          .get("credentials")
          .flatMap(CacheCredentials.read(url.getHost(), _))
      val enableUpload =
        js.obj
          .get("enable-upload")
          .flatMap(_.boolOpt)
          .getOrElse(credentials.isDefined)
      CacheConfig(url, enableUpload, credentials)
    }
  }

  private def computeCacheKey(
      bazel: Bazel,
      project: Project,
      cacheFormatVersion: String
  ): Try[String] = {
    val joinedTargets = project.targets.mkString(" + ")
    bazel
      .query(
        "Computing project cache key",
        s"""filter("^//", buildfiles($joinedTargets) + buildfiles(deps($joinedTargets)))"""
      )
      .map { result =>
        val digest = MessageDigest.getInstance("SHA-256")
        val buildFiles = result
          .getTargetList()
          .asScala
          .toList
          .map { target =>
            val location = target.getSourceFile().getLocation()
            val colonIdx = location.indexOf(':')
            if (colonIdx == -1) location
            else location.substring(0, colonIdx)
          }
          .sorted
          .distinct

        digest.update(cacheFormatVersion.getBytes("UTF-8"))
        project.targets.foreach { target =>
          digest.update(target.getBytes("UTF-8"))
        }

        buildFiles.foreach { buildFile =>
          val path = Paths.get(buildFile)
          if (Files.isReadable(path)) {
            val bytes = Files.readAllBytes(path)
            digest.update(bytes)
          }
        }

        digest
          .digest()
          .map { h =>
            val hex = Integer.toHexString(0xff & h)
            if (hex.length == 1) hex + '0'
            else hex
          }
          .mkString
      }
  }
}
