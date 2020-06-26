package tests

import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath

class BloopPantsSuite extends FastpassSuite {

  val pantsIni: String = """
                           |/pants.ini
                           |[GLOBAL]
                           |pants_version: 1.26.0.dev0
                           |""".stripMargin

  test("create simple build") {
    val workspace = Workspace(s"""$pantsIni
                                 |/core/BUILD
                                 |scala_library(
                                 |  sources=['*.scala'],
                                 |)
                                 |/core/Lib.scala
                                 |package core
                                 |object Lib {
                                 |  def greeting = "Hello from lib!"
                                 |}
                                 |/src/BUILD
                                 |scala_library(
                                 |  sources=['*.scala'],
                                 |  dependencies=['core:core']
                                 |)
                                 |/src/Main.scala
                                 |package src
                                 |object Main extends App {
                                 |  println(core.Lib.greeting)
                                 |}
                                 |""".stripMargin)
    workspace.run("create" :: "::" :: Nil).succeeds
    val projects = workspace.projects()
    assertEquals(projects.keys, Set("-project-root", "core:core", "src:src"))

    projects("src:src")
      .dependsOn(projects("core:core"))
      .hasProjectOnCompileClasspath(projects("core:core"))
      .hasProjectOnRuntimeClasspath(projects("core:core"))
  }

  test("respects strict_deps") {
    val workspace = Workspace(s"""$pantsIni
                                 |/a/BUILD
                                 |scala_library(
                                 |  sources=['*.scala'],
                                 |)
                                 |/a/A.scala
                                 |package a
                                 |object A
                                 |/b/BUILD
                                 |scala_library(
                                 |  sources=['*.scala'],
                                 |  dependencies=['a:a']
                                 |)
                                 |/b/B.scala
                                 |package b
                                 |object B
                                 |/c/BUILD
                                 |scala_library(
                                 |  sources=['*.scala'],
                                 |  dependencies=['b:b'],
                                 |  strict_deps=True
                                 |)
                                 |/c/C.scala
                                 |package c
                                 |object C
                                 |""".stripMargin)

    workspace
      .run(
        "create" :: "--name" :: "test" :: "--strict-deps=strict" :: "c::" :: Nil
      )
      .succeeds
    val projects0 = workspace.projects()
    assertEquals(projects0.keys, Set("c:c", "c-project-root"))
    projects0("c:c")
      .doesntHaveBinaryOnCompileClasspath("a.a.jar")
      .hasBinaryOnCompileClasspath("b.b.jar")
      .hasBinaryOnRuntimeClasspath("a.a.jar")
      .hasBinaryOnRuntimeClasspath("b.b.jar")

    workspace
      .run("amend" :: "test" :: "--new-targets" :: "b::,c::" :: Nil)
      .succeeds
    val projects1 = workspace.projects()
    assertEquals(
      projects1.keys,
      Set("b:b", "b-project-root", "c:c", "c-project-root")
    )
    projects1("b:b")
      .hasBinaryOnCompileClasspath("a.a.jar")
      .hasBinaryOnRuntimeClasspath("a.a.jar")
    projects1("c:c")
      .doesntHaveBinaryOnCompileClasspath("a.a.jar")
      .hasProjectOnCompileClasspath(projects1("b:b"))
      .hasBinaryOnRuntimeClasspath("a.a.jar")
      .hasProjectOnRuntimeClasspath(projects1("b:b"))

    workspace
      .run("amend" :: "test" :: "--new-targets" :: "a::,b::,c::" :: Nil)
      .succeeds
    val projects2 = workspace.projects()
    assertEquals(
      projects2.keys,
      Set("a:a", "a-project-root", "b:b", "b-project-root", "c:c",
        "c-project-root")
    )
    projects2("b:b")
      .hasProjectOnCompileClasspath(projects2("a:a"))
      .hasProjectOnRuntimeClasspath(projects2("a:a"))
    projects2("c:c")
      .doesntHaveProjectOnCompileClasspath(projects2("a:a"))
      .hasProjectOnCompileClasspath(projects2("b:b"))
      .hasProjectOnRuntimeClasspath(projects2("a:a"))
      .hasProjectOnRuntimeClasspath(projects2("b:b"))
  }

  test("ignores jvm_app") {
    val workspace = Workspace(s"""$pantsIni
                                 |/app/BUILD
                                 |scala_library(
                                 |  name="my-library",
                                 |  sources=['my-library/**/*.scala']
                                 |)
                                 |jvm_binary(
                                 |  name="my-binary",
                                 |  dependencies=[':my-library'],
                                 |  main="com.company.Main"
                                 |)
                                 |jvm_app(
                                 |  name="my-bundle",
                                 |  bundles=[bundle(fileset=['config/**/*'])]
                                 |)
                                 |/app/my-library/com/company/Main.scala
                                 |package com.company
                                 |object Main { def main(args: Array[String]): Unit = () }
                                 |/config/config.json
                                 |{ "hello": "world" }
                                 |""".stripMargin)
    workspace
      .run(
        "create" :: "--name" :: "test" :: "::" :: Nil
      )
      .succeeds
    val projects0 = workspace.projects()
    // Ensure this doesn't include the `jvm_app` target:
    assertEquals(
      projects0.keys,
      Set("-project-root", "app:my-binary", "app:my-library")
    )
  }

  test("exports jar_library deps") {
    val workspace = Workspace(s"""$pantsIni
                                 |/libs/BUILD
                                 |scala_library(
                                 |  name = "my-library",
                                 |  sources = ["library/**/*.scala"]
                                 |)
                                 |jar_library(
                                 |    name = "scalacheck",
                                 |    jars = [scala_jar(
                                 |        org = "org.scalacheck",
                                 |        name = "scalacheck",
                                 |        rev = "1.14.0",
                                 |    )],
                                 |    dependencies = [":my-library"]
                                 |)
                                 |/libs/library/Lib.scala
                                 |package library
                                 |object Library { val a = 0 }
                                 |/app/BUILD
                                 |target(
                                 |  name = "proxy",
                                 |  dependencies = ["libs:scalacheck"]
                                 |)
                                 |scala_library(
                                 |  name = "my-binary",
                                 |  sources = ["**/*.scala"],
                                 |  dependencies = [":proxy"],
                                 |  strict_deps = True
                                 |)
                                 |/app/App.scala
                                 |package com.company
                                 |object Main { def main(args: Array[String]): Unit = println("a": library.Library.a) }
                                 |""".stripMargin)
    workspace
      .run(
        "create" :: "--name" :: "test" :: "app:my-binary" :: "--strict-deps=strict" :: Nil
      )
      .succeeds
    val projects0 = workspace.projects()
    assertEquals(projects0.keys, Set("app-project-root", "app:my-binary"))
    projects0("app:my-binary")
      .hasBinaryOnCompileClasspath("libs.my-library.jar")
      .hasBinaryOnRuntimeClasspath("libs.my-library.jar")

    workspace
      .run(
        "amend" :: "test" :: "--new-targets" :: "libs:my-library,app:my-binary" :: "--strict-deps=strict" :: Nil
      )
      .succeeds
    val projects1 = workspace.projects()
    assertEquals(
      projects1.keys,
      Set(
        "app-project-root",
        "app:my-binary",
        "libs-project-root",
        "libs:my-library",
        "libs:scalacheck"
      )
    )
    projects1("app:my-binary")
      .hasProjectOnCompileClasspath(projects1("libs:my-library"))
      .hasProjectOnRuntimeClasspath(projects1("libs:my-library"))

  }

  test("relative sources.jar") {
    val workspace = Workspace(s"""$pantsIni
                                 |/lib/src/main/scala/lib/BUILD
                                 |scala_library()
                                 |/lib/src/main/scala/lib/Hello1.scala
                                 |package lib
                                 |class Hello1
                                 |/lib/src/main/scala/lib/Hello2.scala
                                 |package lib
                                 |class Hello2
                                 |/app/src/main/scala/app/BUILD
                                 |scala_library(
                                 |  dependencies = ["lib/src/main/scala/lib:lib"]
                                 |)
                                 |""".stripMargin)
    workspace.run("create" :: "--name" :: "test" :: "app::" :: Nil).succeeds
    val projects = workspace.projects()
    val app = projects("app/src/main/scala/app:app")
    val List(libSources) = for {
      resolution <- app.resolution.toList
      module <- resolution.modules
      artifact <- module.artifacts
      if artifact.path.endsWith("lib.src.main.scala.lib.lib-sources.jar")
    } yield artifact.path
    val relativePaths =
      FileIO.withJarFileSystem(
        AbsolutePath(libSources),
        create = false,
        close = true
      ) { root =>
        FileIO
          .listAllFilesRecursively(root)
          .files
          .map(_.toURI(false).toString())
      }
    assertNoDiff(
      relativePaths.sorted.mkString("\n"),
      """
        |lib/Hello1.scala
        |lib/Hello2.scala
        |""".stripMargin
    )
  }

}
