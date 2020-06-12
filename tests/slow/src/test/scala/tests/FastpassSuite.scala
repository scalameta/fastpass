package tests

import bloop.config.Config

import tests.build.BuildInfo

import java.io.ByteArrayOutputStream
import java.io.IOException
import java.io.PrintStream
import java.nio.file.DirectoryNotEmptyException
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes

import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.meta.internal.fastpass.SystemProcess
import scala.meta.internal.fastpass.pantsbuild.BloopPants
import scala.meta.io.AbsolutePath

trait FastpassSuite extends munit.FunSuite with BloopAssertions {

  /** A workspace, in which fastpass can be run. */
  class Workspace private (root: AbsolutePath) {

    /** Run fastpass with the given arguments, returns the captured results. */
    def run(command: List[String]): Captured = {
      val outStream = new ByteArrayOutputStream
      val errStream = new ByteArrayOutputStream
      val app = BloopPants.app.copy(
        out = new PrintStream(outStream),
        err = new PrintStream(errStream),
        workingDirectory = root.toNIO
      )

      Workspace.installPants(root)

      val pantsLauncher = BuildInfo.test_pantsCloneLocation.toPath
        .resolve("pants")
      val launcherArgs = List("--pants", pantsLauncher.toString)
      val workspaceArgs = List("--workspace", root.syntax)
      val miscSettings = List("--no-bloop-exit")
      val exitCode =
        app.run(command ++ launcherArgs ++ workspaceArgs ++ miscSettings)

      Captured(
        new String(outStream.toByteArray()),
        new String(errStream.toByteArray()),
        root,
        exitCode
      )
    }

    /** The Bloop configuration files that exist in this workspace. */
    def configFiles(): Iterator[Path] =
      Files
        .list(root.toNIO.resolve(".bloop"))
        .iterator()
        .asScala
        .filter(Workspace.isBloopConfigFile)

    /** The Bloop projects that exist in this workspace. */
    def projects(): Map[String, Config.Project] = {
      configFiles().flatMap { p =>
        bloop.config.read(p) match {
          case Left(err) => throw err
          case Right(config) => List(config.project.name -> config.project)
        }
      }.toMap
    }

  }

  object Workspace {

    /** Setup a workspace with the given source layout */
    def apply(layout: String): Workspace = {
      val root = BuildInfo.test_pantsTestWorkspaceRoot.toPath
      val workspace = AbsolutePath(root.resolve("workspace"))

      Files.createDirectories(root)
      cleanWorkspace(root)
      FileLayout.fromString(layout, root = workspace)

      new Workspace(workspace)
    }

    /** Delete everything under root, except `workspace/.pants.d` */
    private def cleanWorkspace(root: Path): Unit = {
      val workspace = root.resolve("workspace")
      val pantsDir = workspace.resolve(".pants.d")
      Files.walkFileTree(
        root,
        new SimpleFileVisitor[Path] {
          override def preVisitDirectory(
              dir: Path,
              attrs: BasicFileAttributes
          ): FileVisitResult = {
            if (dir == pantsDir) FileVisitResult.SKIP_SUBTREE
            else FileVisitResult.CONTINUE
          }

          override def visitFile(
              file: Path,
              attrs: BasicFileAttributes
          ): FileVisitResult = {
            Files.delete(file)
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(
              dir: Path,
              exc: IOException
          ): FileVisitResult = {
            if (dir != root && dir != workspace && dir != pantsDir) {
              try Files.delete(dir)
              catch {
                case _: DirectoryNotEmptyException => ()
              } // Happens sometimes on Windows?
            }
            FileVisitResult.CONTINUE
          }
        }
      )
    }

    private def isBloopConfigFile(p: Path) = {
      val name = p.getFileName.toString
      name.endsWith(".json") && name != "bloop.settings.json"
    }

    private def installPants(directory: AbsolutePath): Unit = {
      clonePants(
        BuildInfo.test_pantsCloneLocation.toPath,
        BuildInfo.test_pantsCloneUrl,
        BuildInfo.test_pantsCloneBranch
      )
      installPantsLauncher(directory)
    }

    private def clonePants(
        destination: Path,
        url: String,
        branch: String
    ): Unit = {
      import scala.concurrent.ExecutionContext.Implicits.global
      if (!Files.exists(destination)) {
        Files.createDirectories(destination)
        val args =
          List(
            "git",
            "clone",
            url,
            "--branch",
            branch,
            "--depth",
            "1",
            "."
          )
        SystemProcess.run(
          "git clone",
          args,
          args,
          destination,
          () => (),
          System.out,
          System.err
        )
      } else if (BuildInfo.insideCI) {
        val args = List("git", "pull", "origin", branch)
        SystemProcess.run(
          "git pull",
          args,
          args,
          destination,
          () => (),
          System.out,
          System.err
        )
      }
    }

    private def installPantsLauncher(directory: AbsolutePath): Unit = {
      val pantsStream = getClass.getClassLoader.getResourceAsStream("pants")
      val pants = directory.toNIO.resolve("pants")
      if (!Files.exists(pants)) {
        Files.copy(pantsStream, pants)
        pants.toFile.setExecutable(true)
      }
    }

  }

  case class Captured(
      out: String,
      err: String,
      workingDirectory: AbsolutePath,
      exitCode: Int
  ) {
    def succeeds: Captured = {
      assertEquals(exitCode, 0, out + err)
      this
    }
  }
}
