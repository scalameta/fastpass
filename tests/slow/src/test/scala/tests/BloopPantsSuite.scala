package tests

import java.nio.charset.StandardCharsets

import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath

import bloop.config.Config

class BloopPantsSuite extends FastpassSuite {

  val pantsIni: String = """
                           |/pants.ini
                           |[GLOBAL]
                           |pants_version: 1.26.0.dev0
                           |[resolver]
                           |resolver: coursier
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
    workspace
      .run("create" :: "--name" :: "example/project" :: "::" :: Nil)
      .succeeds
    val projects = workspace.projects()
    assertEquals(projects.keys, Set("-project-root", "core:core", "src:src"))

    projects("src:src")
      .dependsOn(projects("core:core"))
      .hasProjectsOnCompileClasspath(projects("core:core"))
      .hasProjectsOnRuntimeClasspath(projects("core:core"))
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
    assertEquals(projects0.keys, Set("c:c"))
    projects0("c:c")
      .doesntHaveBinariesOnCompileClasspath("a.a.jar")
      .hasBinariesOnCompileClasspath("b.b.jar")
      .hasBinariesOnRuntimeClasspath("a.a.jar", "b.b.jar")

    workspace
      .run("amend" :: "test" :: "--new-targets" :: "b::,c::" :: Nil)
      .succeeds
    val projects1 = workspace.projects()
    assertEquals(
      projects1.keys,
      Set("b:b", "c:c")
    )
    projects1("b:b")
      .hasBinariesOnCompileClasspath("a.a.jar")
      .hasBinariesOnRuntimeClasspath("a.a.jar")
    projects1("c:c")
      .doesntHaveBinariesOnCompileClasspath("a.a.jar")
      .hasProjectsOnCompileClasspath(projects1("b:b"))
      .hasBinariesOnRuntimeClasspath("a.a.jar")
      .hasProjectsOnRuntimeClasspath(projects1("b:b"))

    workspace
      .run("amend" :: "test" :: "--new-targets" :: "a::,b::,c::" :: Nil)
      .succeeds
    val projects2 = workspace.projects()
    assertEquals(
      projects2.keys,
      Set("a:a", "b:b", "c:c")
    )
    projects2("b:b")
      .hasProjectsOnCompileClasspath(projects2("a:a"))
      .hasProjectsOnRuntimeClasspath(projects2("a:a"))
    projects2("c:c")
      .doesntHaveProjectsOnCompileClasspath(projects2("a:a"))
      .hasProjectsOnCompileClasspath(projects2("b:b"))
      .hasProjectsOnRuntimeClasspath(projects2("b:b"), projects2("a:a"))
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
    assertEquals(projects0.keys, Set("app:my-binary"))
    projects0("app:my-binary")
      .hasBinariesOnCompileClasspath("libs.my-library.jar")
      .hasBinariesOnRuntimeClasspath("libs.my-library.jar")

    workspace
      .run(
        "amend" :: "test" :: "--new-targets" :: "libs:my-library,app:my-binary" :: "--strict-deps=strict" :: Nil
      )
      .succeeds
    val projects1 = workspace.projects()
    assertEquals(
      projects1.keys,
      Set(
        "app:my-binary",
        "libs:my-library",
        "libs:scalacheck"
      )
    )
    projects1("app:my-binary")
      .hasProjectsOnCompileClasspath(projects1("libs:my-library"))
      .hasProjectsOnRuntimeClasspath(projects1("libs:my-library"))

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
        |META-INF/fastpass/source-root
        |lib/Hello1.scala
        |lib/Hello2.scala
        |""".stripMargin
    )

    val sourceRootContents = {
      FileIO.withJarFileSystem(
        AbsolutePath(libSources),
        create = false,
        close = true
      ) { root =>
        val bytes = FileIO
          .readAllBytes(root.resolve("META-INF/fastpass/source-root"))
        new String(bytes, StandardCharsets.UTF_8)
      }
    }

    assertNoDiff(
      "lib/src/main/scala",
      sourceRootContents
    )

  }

  test("specify main_class") {
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
                                 |/app/my-library/com/company/Main.scala
                                 |package com.company
                                 |object Main { def main(args: Array[String]): Unit = () }
                                 |""".stripMargin)
    workspace.run("create" :: "--name" :: "test" :: "app::" :: Nil).succeeds
    val projects = workspace.projects()
    val binary = projects("app:my-binary")
    binary.platform match {
      case Some(jvm: Config.Platform.Jvm) =>
        assertEquals(jvm.mainClass, Some("com.company.Main"))
      case _ =>
        fail("No main class specified.")
    }
  }

  test("globs are translated") {
    val workspace = Workspace(s"""$pantsIni
                                 |/app/BUILD
                                 |scala_library(
                                 |  name="my-library",
                                 |  sources=[
                                 |    'foo/**/*.scala',
                                 |    'bar/*',
                                 |    'baz/**/*/foo/*',
                                 |    '!fizz/**/*.scala',
                                 |    '!buzz/*',
                                 |    '!fizz/**/*/buzz/*',
                                 |  ]
                                 |)""".stripMargin)
    workspace
      .run("create" :: "--name" :: "test" :: "app:my-library" :: Nil)
      .succeeds
    val projects = workspace.projects()
    val library = projects("app:my-library")
    val expectedIncludes =
      "glob:foo/**.scala" :: "glob:bar/**" :: "glob:baz/**/foo/**" :: Nil
    val expectedExcludes =
      "glob:fizz/**.scala" :: "glob:buzz/**" :: "glob:fizz/**/buzz/**" :: Nil
    val actualIncludes = library.sourcesGlobs.getOrElse(Nil).flatMap(_.includes)
    val actualExcludes = library.sourcesGlobs.getOrElse(Nil).flatMap(_.excludes)

    assertEquals(actualIncludes, expectedIncludes)
    assertEquals(actualExcludes, expectedExcludes)
  }

  test("respect excludes in runtime classpath") {
    val workspace = Workspace(s"""$pantsIni
                                 |/lib/BUILD
                                 |jar_library(
                                 |    name = "scalacheck",
                                 |    jars = [scala_jar(
                                 |        org = "org.scalacheck",
                                 |        name = "scalacheck",
                                 |        rev = "1.14.0",
                                 |    )]
                                 |)
                                 |scala_library(
                                 |  name="lib0",
                                 |  sources=['lib0/**/*.scala'],
                                 |  dependencies=[':scalacheck'],
                                 |  excludes=[exclude(org='org.scala-sbt', name='test-interface')]
                                 |)
                                 |""".stripMargin)
    workspace.run("create" :: "--name" :: "test" :: "lib::" :: Nil).succeeds
    val projects = workspace.projects()

    projects("lib:lib0")
      .hasBinariesOnCompileClasspath("test-interface-1.0.jar")
      .doesntHaveBinariesOnRuntimeClasspath("test-interface-1.0.jar")
  }

  test("order classpath using BFS") {
    val workspace = Workspace(s"""$pantsIni
                                 |/lib/BUILD
                                 |scala_library(
                                 |  name="a",
                                 |  sources=['a/**/*.scala'],
                                 |  dependencies=[':b', ':c']
                                 |)
                                 |scala_library(
                                 |  name="b",
                                 |  sources=['b/**/*.scala'],
                                 |  dependencies=[':d']
                                 |)
                                 |scala_library(
                                 |  name="c",
                                 |  sources=['c/**/*.scala'],
                                 |  dependencies=[':f']
                                 |)
                                 |scala_library(
                                 |  name="d",
                                 |  sources=['d/**/*.scala'],
                                 |  dependencies=[':e']
                                 |)
                                 |scala_library(
                                 |  name="e",
                                 |  sources=['e/**/*.scala']
                                 |)
                                 |scala_library(
                                 |  name="f",
                                 |  sources=['f/**/*.scala'],
                                 |  dependencies=[':g']
                                 |)
                                 |scala_library(
                                 |  name="g",
                                 |  sources=['g/**/*.scala']
                                 |)
                                 |""".stripMargin)
    workspace.run("create" :: "--name" :: "test" :: "lib::" :: Nil).succeeds
    val projects = workspace.projects()
    val a = projects("lib:a")
    val b = projects("lib:b")
    val c = projects("lib:c")
    val d = projects("lib:d")
    val e = projects("lib:e")
    val f = projects("lib:f")
    val g = projects("lib:g")

    a.hasProjectsOnCompileClasspath(b, c, d, f, e, g)
    b.hasProjectsOnCompileClasspath(d, e)
      .doesntHaveProjectsOnCompileClasspath(a, c, f, g)
    c.hasProjectsOnCompileClasspath(f, g)
      .doesntHaveProjectsOnCompileClasspath(a, b, d, e)
    d.hasProjectsOnCompileClasspath(e)
      .doesntHaveProjectsOnCompileClasspath(a, b, c, f, g)
    e.doesntHaveProjectsOnCompileClasspath(a, b, c, f, g)
    f.hasProjectsOnCompileClasspath(g)
      .doesntHaveProjectsOnCompileClasspath(a, b, c, d, e)
    g.doesntHaveProjectsOnCompileClasspath(a, b, c, d, e, f)

  }

  test("order classpath using BFS with strict deps") {
    val workspace = Workspace(s"""$pantsIni
                                 |/lib/BUILD
                                 |scala_library(
                                 |  name="a",
                                 |  sources=['a/**/*.scala'],
                                 |  dependencies=[':b', ':d'],
                                 |  strict_deps=True
                                 |)
                                 |scala_library(
                                 |  name="b",
                                 |  sources=['b/**/*.scala'],
                                 |  dependencies=[':c'],
                                 |  exports=[':c'],
                                 |  strict_deps=True
                                 |)
                                 |scala_library(
                                 |  name="c",
                                 |  sources=['c/**/*.scala'],
                                 |  strict_deps=True
                                 |)
                                 |scala_library(
                                 |  name="d",
                                 |  sources=['d/**/*.scala'],
                                 |  dependencies=[':e'],
                                 |  exports=[":e"],
                                 |  strict_deps=True
                                 |)
                                 |scala_library(
                                 |  name="e",
                                 |  sources=['e/**/*.scala'],
                                 |  dependencies=[':f'],
                                 |  exports=[':f'],
                                 |  strict_deps=True
                                 |)
                                 |scala_library(
                                 |  name="f",
                                 |  sources=['f/**/*.scala'],
                                 |  strict_deps=True
                                 |)
                                 |""".stripMargin)
    workspace.run("create" :: "--name" :: "test" :: "lib::" :: Nil).succeeds
    val projects = workspace.projects()

    val a = projects("lib:a")
    val b = projects("lib:b")
    val c = projects("lib:c")
    val d = projects("lib:d")
    val e = projects("lib:e")
    val f = projects("lib:f")

    a.hasProjectsOnCompileClasspath(b, d, c, e, f)
    b.hasProjectsOnCompileClasspath(c)
      .doesntHaveProjectsOnCompileClasspath(a, d, e, f)
    c.doesntHaveProjectsOnCompileClasspath(a, b, d, e, f)
    d.hasProjectsOnCompileClasspath(e, f)
      .doesntHaveProjectsOnCompileClasspath(a, b, c)
    e.hasProjectsOnCompileClasspath(f)
      .doesntHaveProjectsOnCompileClasspath(a, b, c, d)
    f.doesntHaveProjectsOnCompileClasspath(a, b, c, d, e)
  }

}
