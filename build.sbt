lazy val V = new {
  val scala212 = "2.12.11"
  val bloop = "1.4.1"
  val coursierInterfaces = "0.0.22"
  val scribe = "2.7.12"
  val ujson = "1.1.0"
  val metaconfig = "0.9.10"
  val scalameta = "4.3.10"
  val bsp = "2.0.0-M4+10-61e61e87"
  val metals = "0.9.0"
}

onLoad.in(Global) ~= { old =>
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

lazy val fastpass = project
  .in(file("."))
  .enablePlugins(BuildInfoPlugin, GraalVMNativeImagePlugin)
  .settings(
    organization := "org.scalameta",
    name := "fastpass",
    scalaVersion := V.scala212,
    libraryDependencies ++= Seq(
      "com.geirsson" %% "metaconfig-core" % V.metaconfig,
      "io.get-coursier" % "interface" % V.coursierInterfaces,
      "com.outr" %% "scribe" % V.scribe,
      "com.lihaoyi" %% "ujson" % V.ujson,
      "ch.epfl.scala" %% "bloop-frontend" % V.bloop,
      "ch.epfl.scala" %% "bloop-config" % V.bloop,
      "ch.epfl.scala" %% "bloop-launcher" % V.bloop,
      "org.scalameta" %% "trees" % V.scalameta,
      "ch.epfl.scala" % "bsp4j" % V.bsp
    ),
    buildInfoPackage := "scala.meta.internal.fastpass",
    buildInfoKeys := Seq[BuildInfoKey](
      "metalsVersion" -> V.metals,
      "fastpassVersion" -> version.value,
      "bspVersion" -> V.bsp,
      "bloopVersion" -> V.bloop,
      "bloopNightlyVersion" -> V.bloop,
      "scala212" -> V.scala212
    ),
    mainClass in GraalVMNativeImage := Some(
      "scala.meta.internal.fastpass.pantsbuild.BloopPants"
    ),
    graalVMNativeImageOptions ++= {
      val reflectionFile =
        Keys.sourceDirectory.in(Compile).value./("graal")./("reflection.json")
      assert(reflectionFile.exists, "no such file: " + reflectionFile)
      List(
        "-H:+ReportUnsupportedElementsAtRuntime",
        "--initialize-at-build-time",
        "--initialize-at-run-time=scala.meta.fastpass,metaconfig",
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

addCommandAlias(
  "fastpass-link",
  "; fastpass/graalvm-native-image:packageBin ; taskready"
)
commands += Command.command("taskready") { s =>
  import scala.sys.process._
  "say 'native-image ready'".!
  s
}
