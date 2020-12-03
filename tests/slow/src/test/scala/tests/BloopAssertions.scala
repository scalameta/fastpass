package tests

import java.nio.file.Path
import java.nio.file.Paths

import bloop.config.Config.Platform
import bloop.config.Config.Project

trait BloopAssertions extends munit.Assertions {
  implicit class ProjectAssertions(val project: Project) {

    def dependsOn(others: Project*): Project =
      chain {
        others.foreach { other =>
          assert(
            project.dependencies.contains(other.name),
            s"`${project.name}` doesn't depend on `${other.name}`."
          )
        }
      }

    def doesntDependOn(others: Project*): Project =
      chain {
        others.foreach { other =>
          assert(
            !project.dependencies.contains(other.name),
            s"`${project.name}` depends on `${other.name}`."
          )
        }
      }

    def doesntHaveBinariesOnCompileClasspath(jars: String*): Project =
      chain {
        jars.foreach(assertClasspathDoesntContain(project.classpath, _))
      }

    def hasBinariesOnCompileClasspath(
        jars: String*
    )(implicit loc: munit.Location): Project =
      chain {
        jars.foreach(assertClasspathContains(project.classpath, _))
      }

    def doesntHaveBinariesOnRuntimeClasspath(jars: String*): Project =
      chain {
        jars.foreach { jar =>
          onRuntimeClasspath(assertClasspathDoesntContain(_, jar))
        }
      }

    def hasBinariesOnRuntimeClasspath(jars: String*): Project =
      chain {
        jars.foreach { jar =>
          onRuntimeClasspath(assertClasspathContains(_, jar))
        }
      }

    def doesntHaveProjectsOnCompileClasspath(others: Project*): Project =
      chain {
        others.foreach { other =>
          assertClasspathDoesntContain(project.classpath, other.classesDir)
        }
      }

    def hasProjectsOnCompileClasspath(others: Project*): Project =
      chain {
        others.foreach { other =>
          assertClasspathContains(project.classpath, other.classesDir)
        }
        val othersIndices =
          others.map(p => project.classpath.indexOf(p.classesDir))
        assert(othersIndices == othersIndices.sorted)
      }

    def doesntHaveProjectsOnRuntimeClasspath(others: Project*): Project =
      chain {
        others.foreach { other =>
          onRuntimeClasspath(assertClasspathDoesntContain(_, other.classesDir))
        }
      }

    def hasProjectsOnRuntimeClasspath(others: Project*): Project =
      chain {
        onRuntimeClasspath { classpath =>
          others.foreach { other =>
            assertClasspathContains(classpath, other.classesDir)
          }
          val othersIndices = others.map(p => classpath.indexOf(p.classesDir))
          assert(othersIndices == othersIndices.sorted)
        }
      }

    private def onRuntimeClasspath(op: List[Path] => Unit): Unit = {
      project.platform match {
        case Some(Platform.Jvm(_, _, _, Some(runtimeClasspath), _)) =>
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

  def assertClasspathContains(classpath: List[Path], jar: Path)(implicit
      loc: munit.Location
  ): Unit =
    assert(
      classpath.exists(_.endsWith(jar)),
      s"`$jar` doesn't appear on classpath: ${classpath.mkString("\n - ", "\n - ", "")}"
    )

  def assertClasspathContains(classpath: List[Path], jar: String)(implicit
      loc: munit.Location
  ): Unit =
    assertClasspathContains(classpath, Paths.get(jar))
}
