package tests

import bloop.config.Config.Project
import bloop.config.Config.Platform

import java.nio.file.Path
import java.nio.file.Paths

trait BloopAssertions extends munit.Assertions {
  implicit class ProjectAssertions(val project: Project) {

    def dependsOn(other: Project): Project =
      chain {
        assert(
          project.dependencies.contains(other.name),
          s"`${project.name}` doesn't depend on `${other.name}`."
        )
      }

    def doesntDependOn(other: Project): Project =
      chain {
        assert(
          !project.dependencies.contains(other.name),
          s"`${project.name}` depends on `${other.name}`."
        )
      }

    def doesntHaveBinaryOnCompileClasspath(jar: String): Project =
      chain {
        assertClasspathDoesntContain(project.classpath, jar)
      }

    def hasBinaryOnCompileClasspath(jar: String): Project =
      chain {
        assertClasspathContains(project.classpath, jar)
      }

    def doesntHaveBinaryOnRuntimeClasspath(jar: String): Project =
      chain {
        onRuntimeClasspath(assertClasspathDoesntContain(_, jar))
      }

    def hasBinaryOnRuntimeClasspath(jar: String): Project =
      chain {
        onRuntimeClasspath(assertClasspathContains(_, jar))
      }

    def doesntHaveProjectOnCompileClasspath(other: Project): Project =
      chain {
        assertClasspathDoesntContain(project.classpath, other.classesDir)
      }

    def hasProjectOnCompileClasspath(other: Project): Project =
      chain {
        assertClasspathContains(project.classpath, other.classesDir)
      }

    def doesntHaveProjectOnRuntimeClasspath(other: Project): Project =
      chain {
        onRuntimeClasspath(assertClasspathDoesntContain(_, other.classesDir))
      }

    def hasProjectOnRuntimeClasspath(other: Project): Project =
      chain {
        onRuntimeClasspath(assertClasspathContains(_, other.classesDir))
      }

    private def onRuntimeClasspath(op: List[Path] => Unit): Unit = {
      project.platform match {
        case Some(Platform.Jvm(_, _, Some(runtimeClasspath), _)) =>
          op(runtimeClasspath)
        case _ =>
          op(project.classpath)
      }
    }

    private def chain(op: => Unit): Project = { op; project }
  }

  def assertClasspathDoesntContain(classpath: List[Path], jar: Path): Unit =
    assert(
      !classpath.exists(_.endsWith(jar)),
      s"`$jar` appears on classpath: ${classpath.mkString("\n - ", "\n - ", "")}"
    )

  def assertClasspathDoesntContain(classpath: List[Path], jar: String): Unit =
    assertClasspathDoesntContain(classpath, Paths.get(jar))

  def assertClasspathContains(classpath: List[Path], jar: Path): Unit =
    assert(
      classpath.exists(_.endsWith(jar)),
      s"`$jar` doesn't appear on classpath: ${classpath.mkString("\n - ", "\n - ", "")}"
    )

  def assertClasspathContains(classpath: List[Path], jar: String): Unit =
    assertClasspathContains(classpath, Paths.get(jar))
}
