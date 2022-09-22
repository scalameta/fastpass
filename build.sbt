lazy val V = new {
  val scala212 = "2.12.15"
  val bloop = "1.5.3-42-75a0d6ee"
  val coursierInterfaces = "1.0.6"
  val scribe = "2.7.12"
  val ujson = "1.1.0"
  val metaconfig = "0.9.10"
  val scalameta = "4.4.0"
  val bsp = "2.0.0-M4+10-61e61e87"
  val munit = "0.7.7"
  val xz = "1.9"
}

Global / onLoad ~= { old =>
  if (!scala.util.Properties.isWin) {
    import java.nio.file._
    val prePush = Paths.get(".git", "hooks", "pre-push")
    Files.createDirectories(prePush.getParent)
    Files.write(
      prePush,
      """#!/bin/sh
        |set -eux
        |bin/scalafmt --diff
        |git diff --exit-code
        |""".stripMargin.getBytes()
    )
    prePush.toFile.setExecutable(true)
  }
  old
}

inThisBuild(
  List(
    organization := "org.scalameta",
    homepage := Some(url("https://github.com/scalameta/fastpass")),
    licenses := Seq(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "olafurpg",
        "Ólafur Páll Geirsson",
        "olafurpg@gmail.com",
        url("https://geirsson.com")
      ),
      Developer(
        "wiwa",
        "Win Wang",
        "winw@twitter.com",
        url("https://github.com/wiwa")
      ),
      Developer(
        "Duhemm",
        "Martin Duhem",
        "martin.duhem@gmail.com",
        url("https://github.com/Duhemm")
      )
    ),
    scalaVersion := V.scala212,
    crossScalaVersions := List(V.scala212),
    scalacOptions ++= List(
      "-target:jvm-1.8",
      "-Yrangepos",
      // -Xlint is unusable because of
      // https://github.com/scala/bug/issues/10448
      "-Ywarn-unused:imports"
    ),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixCaching := true,
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"
  )
)

publish / skip := true
crossScalaVersions := Nil

addCommandAlias(
  "native-image",
  "; fastpass/graalvm-native-image:packageBin ; taskready"
)

addCommandAlias(
  "fix",
  "; all scalafix test:scalafix ; all scalafmt test:scalafmt scalafmtSbt"
)

addCommandAlias("fixCheck", "; scalafix --check ; test:scalafix --check")

commands += Command.command("taskready") { s =>
  import scala.sys.process._
  if (System.getenv("CI") == null) {
    scala.util.Try("say 'native-image ready'".!)
  }
  s
}

lazy val fastpass = project
  .in(file("fastpass"))
  .enablePlugins(BuildInfoPlugin, GraalVMNativeImagePlugin, ProtobufPlugin)
  .settings(
    organization := "org.scalameta",
    name := "fastpass",
    scalaVersion := V.scala212,
    fork := true,
    libraryDependencies ++= Seq(
      "com.geirsson" %% "metaconfig-core" % V.metaconfig,
      "io.get-coursier" % "interface" % V.coursierInterfaces,
      "com.outr" %% "scribe" % V.scribe,
      "com.lihaoyi" %% "ujson" % V.ujson,
      "ch.epfl.scala" %% "bloop-frontend" % V.bloop,
      "ch.epfl.scala" %% "bloop-config" % V.bloop,
      "org.scalameta" %% "trees" % V.scalameta,
      "ch.epfl.scala" % "bsp4j" % V.bsp,
      "org.tukaani" % "xz" % V.xz
    ),
    buildInfoPackage := "scala.meta.internal.fastpass",
    buildInfoKeys := Seq[BuildInfoKey](
      "fastpassVersion" -> version.value,
      "bspVersion" -> V.bsp,
      "bloopVersion" -> V.bloop,
      "bloopNightlyVersion" -> V.bloop,
      "scala212" -> V.scala212
    ),
    Compile / mainClass := Some(
      "scala.meta.fastpass.Fastpass"
    ),
    GraalVMNativeImage / mainClass := Some(
      "scala.meta.fastpass.Fastpass"
    ),
    graalVMNativeImageCommand ~= { old =>
      import scala.util.Try
      import java.nio.file.Paths
      import scala.sys.process._
      Try {
        val jabba = Paths
          .get(sys.props("user.home"))
          .resolve(".jabba")
          .resolve("bin")
          .resolve("jabba")
        val home = s"$jabba which --home graalvm-ce-java11@20.1.0".!!.trim()
        Paths.get(home).resolve("bin").resolve("native-image").toString
      }.getOrElse(old)
    },
    graalVMNativeImageOptions ++= {
      val reflectionFile =
        (Compile / sourceDirectory).value / "graal" / "reflection.json"
      assert(reflectionFile.exists, "no such file: " + reflectionFile)
      List(
        "--initialize-at-build-time",
        "--initialize-at-run-time=scala.meta.internal.fastpass,metaconfig,scala.util.Random,coursierapi.internal.jniutils.ApiInternalNativeApi",
        "--no-server",
        "--enable-http",
        "--enable-https",
        "-H:EnableURLProtocols=http,https",
        "--enable-all-security-services",
        "--no-fallback",
        s"-H:ReflectionConfigurationFiles=$reflectionFile",
        "--allow-incomplete-classpath",
        "-H:+ReportExceptionStackTraces"
      )
    }
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .enablePlugins(JmhPlugin)
  .dependsOn(fastpass)
  .settings(
    publish / skip := true
  )
