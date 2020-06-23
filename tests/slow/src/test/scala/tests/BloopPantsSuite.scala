package tests

class BloopPantsSuite extends FastpassSuite {

  val pantsIni = """
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

}
