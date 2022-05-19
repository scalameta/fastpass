package scala.meta.internal.fastpass.generic

import java.nio.file.Path

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.util.Try

import coursierapi.Dependency
import coursierapi.MavenRepository

object DependencyResolution {
  lazy val testingFrameworkJars: Try[List[Path]] = {
    // NOTE(olafur) This is a fork of the official sbt JUnit testing interface
    // https://github.com/scalameta/munit/tree/master/junit-interface that
    // reproduces the JUnit test runner in Pants. Most importantly, it
    // automatically registers org.scalatest.junit.JUnitRunner and
    // org.scalatestplus.junit.JUnitRunner even if there is no `@RunWith`
    // annotation.
    val junitInterface =
      Dependency.of("org.scalameta", "junit-interface", "1.0.0-M3")
    fetchDependencies(junitInterface :: Nil)
  }

  lazy val scalaJars: Try[List[Path]] = {
    val scalaCompiler =
      Dependency.of("org.scala-lang", "scala-compiler", "2.12.15")
    val collectionCompat = Dependency.of(
      "org.scala-lang.modules",
      "scala-collection-compat_2.12",
      "2.4.3"
    )
    fetchDependencies(scalaCompiler :: collectionCompat :: Nil)
  }

  // See https://github.com/scalatest/scalatest/pull/1739
  private def fetchDependencies(deps: Seq[Dependency]): Try[List[Path]] =
    Try {
      coursierapi.Fetch
        .create()
        .withDependencies(deps: _*)
        .addRepositories(
          MavenRepository.of(
            "https://oss.sonatype.org/content/repositories/public"
          )
        )
        .fetch()
        .asScala
        .map(_.toPath())
        .toList
    }
}
