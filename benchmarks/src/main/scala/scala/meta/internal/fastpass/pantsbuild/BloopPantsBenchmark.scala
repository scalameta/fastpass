package scala.meta.internal.fastpass.pantsbuild

import java.util.concurrent.TimeUnit

import scala.meta.internal.fastpass.generic.ExportOptions
import scala.meta.internal.fastpass.generic.ImportMode
import scala.meta.internal.fastpass.generic.OpenOptions
import scala.meta.internal.fastpass.generic.Project
import scala.meta.internal.fastpass.generic.ProjectRoot
import scala.meta.internal.fastpass.generic.SharedOptions
import scala.meta.internal.fastpass.generic.SourcesMode
import scala.meta.internal.fastpass.pantsbuild.commands.StrictDepsMode
import scala.meta.io.AbsolutePath

import metaconfig.cli.CliApp
import org.openjdk.jmh.annotations._

private object BloopPantsBenchmark {

  @State(Scope.Thread)
  class BenchmarkState {
    val pantsExport: PantsExport = {
      val root = ProjectRoot(AbsolutePath.workingDirectory)
      val project = Project(
        SharedOptions(),
        "name",
        Nil,
        root,
        SourcesMode.Default,
        StrictDepsMode.Default,
        ImportMode.Pants
      )
      val app = CliApp("", "", Nil)
      val export = Export(project, OpenOptions(), app, export = ExportOptions())
      val jsonBytes = {
        val baos = new java.io.ByteArrayOutputStream
        val stream = getClass.getResourceAsStream("/export.json")
        try {
          var read = 0
          val buffer = new Array[Byte](1024)
          while ({ read = stream.read(buffer); read } != -1)
            baos.write(buffer, 0, read)
        } finally stream.close()
        baos.toByteArray()
      }
      val json = ujson.read(jsonBytes)
      PantsExport.fromJson(export, json)
    }

    val targets: List[PantsTarget] =
      pantsExport.targets.values.toList.sortBy(_.name)
  }
}

class BloopPantsBenchmark {
  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def runtimeDependencies(state: BloopPantsBenchmark.BenchmarkState): Unit = {
    val runtimeBFS = new RuntimeBFS(state.pantsExport, RuntimeScope)
    state.targets.foreach(runtimeBFS.dependencies)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def compileDependenciesStrict(
      state: BloopPantsBenchmark.BenchmarkState
  ): Unit = {
    val compileBFS = new CompileBFS(state.pantsExport, StrictDepsMode.Strict)
    state.targets.foreach(compileBFS.dependencies)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.SingleShotTime))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def compileDependenciesPlusOne(
      state: BloopPantsBenchmark.BenchmarkState
  ): Unit = {
    val compileBFS = new CompileBFS(state.pantsExport, StrictDepsMode.PlusOne)
    state.targets.foreach(compileBFS.dependencies)
  }
}
