package scala.meta.internal.fastpass.bazelbuild

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Base64

import scala.collection.JavaConverters.asScalaBufferConverter

final case class Credentials(
    username: String,
    password: String
) {
  def encoded: String = {
    val bytes = s"$username:$password".getBytes("UTF-8")
    "Basic " + new String(Base64.getEncoder.encode(bytes));
  }
}

object CacheCredentials {

  private case class Machine(
      host: String,
      username: String,
      password: String
  )

  def read(
      host: String,
      js: ujson.Value
  ): Option[Credentials] = {
    js.arr.foldLeft(Option.empty[Credentials]) {
      case (acc, entry) =>
        acc.orElse(filterCred(host, entry))
    }
  }

  private def filterCred(
      host: String,
      cred: ujson.Value
  ): Option[Credentials] = {
    for {
      tpe <- cred.obj.get("type").map(_.str)
      path <- cred.obj.get("path").map(p => Paths.get(p.str))
      if Files.isReadable(path)
      credentials <- getCredentials(host, tpe, path)
    } yield credentials
  }

  private val yamlCredentials = """(.+?): (.+?)""".r
  private def getCredentials(
      host: String,
      tpe: String,
      path: Path
  ): Option[Credentials] =
    tpe match {
      case "yaml" =>
        Files.readAllLines(path).asScala.collectFirst {
          case yamlCredentials(username, password) =>
            Credentials(username, password)
        }
      case "netrc" =>
        val machines = parseNetRc(
          Nil,
          Files
            .readAllLines(path)
            .asScala
            .mkString(" ")
            .split(" ")
            .filterNot(_.isEmpty)
            .toList
        )
        machines
          .find(_.host == host)
          .orElse(machines.find(_.host == "default"))
          .map {
            case Machine(_, username, password) =>
              Credentials(username, password)
          }

      case _ =>
        None
    }

  @scala.annotation.tailrec
  private def parseNetRc(
      acc: List[Machine],
      tokens: List[String]
  ): List[Machine] =
    (acc, tokens) match {
      case (_, "default" :: rest) =>
        parseNetRc(Machine("default", "", "") :: acc, rest)
      case (_, "machine" :: name :: rest) =>
        parseNetRc(Machine(name, "", "") :: acc, rest)
      case (curr :: machines, "login" :: login :: rest) =>
        parseNetRc(curr.copy(username = login) :: machines, rest)
      case (curr :: machines, "password" :: password :: rest) =>
        parseNetRc(curr.copy(password = password) :: machines, rest)
      case (_, _ :: rest) =>
        parseNetRc(acc, rest)
      case (_, Nil) =>
        acc
    }

}
