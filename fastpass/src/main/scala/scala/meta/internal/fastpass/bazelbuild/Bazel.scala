package scala.meta.internal.fastpass.bazelbuild

import java.io.ByteArrayOutputStream
import java.io.OutputStream
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.sys.process.Process
import scala.sys.process.ProcessIO
import scala.util.Try

import scala.meta.internal.fastpass.FileUtils
import scala.meta.internal.fastpass.MessageOnlyException
import scala.meta.internal.fastpass.bazelbuild.AnalysisProtosV2.ActionGraphContainer
import scala.meta.internal.fastpass.bazelbuild.Build.QueryResult
import scala.meta.internal.fastpass.bazelbuild.Build.Target
import scala.meta.internal.fastpass.console.ProgressConsole

class Bazel(bazelPath: Path, cwd: Path) {
  private val bazel: String = {
    val path = cwd.relativize(bazelPath)
    Option(path.getParent) match {
      case Some(parent) => path.toString
      case None => Paths.get(".").resolve(path).toString
    }
  }

  def info(): Try[BazelInfo] = {
    ProgressConsole
      .auto("Gathering basic build information") { err =>
        val out = new ByteArrayOutputStream
        val cmd = bazel :: "info" :: Nil
        val code = run(cmd, out, err)
        if (code == 0) BazelInfo.fromOutput(out.toString("UTF-8"))
        else throw new MessageOnlyException("`bazel info` failed.")
      }
      .flatten
  }

  def query(title: String, expr: String): Try[QueryResult] = {
    ProgressConsole.auto(title) { err =>
      val out = new ByteArrayOutputStream
      val cmd = List(
        bazel,
        "query",
        expr,
        "--noimplicit_deps",
        "--notool_deps",
        "--output=proto"
      )
      if (run(cmd, out, err) != 0) {
        throw new MessageOnlyException(title + " failed")
      }
      QueryResult.parseFrom(out.toByteArray())
    }
  }

  def build(
      targets: List[String],
      outputGroups: Seq[String] = Nil
  ): Try[Unit] = {
    val title = "Building project dependencies"
    ProgressConsole.auto(
      title,
      progress = new BazelProgress()
    ) { err =>
      err.println("Targets to build:")
      targets.foreach(tgt => err.println(" - " + tgt))

      val out = new ByteArrayOutputStream
      val outputGroupsArg =
        if (outputGroups.isEmpty) None
        else Some(outputGroups.mkString("--output_groups=+", ",+", ""))
      val code =
        // If there are more than 100 targets, specify the targets to build
        // using a file to avoid going over the max arg length.
        if (targets.length < 100) {
          run(
            List(bazel, "build") ++ outputGroupsArg ++ targets,
            out,
            err
          )
        } else {
          FileUtils.withTempFile { tmp =>
            val bytes = targets.mkString(System.lineSeparator).getBytes("UTF-8")
            Files.write(tmp, bytes)
            run(
              List(
                bazel,
                "build",
                s"--target_pattern_file=$tmp"
              ) ++ outputGroupsArg,
              out,
              err
            )
          }
        }
      if (code != 0) {
        throw new MessageOnlyException("Build failed")
      }
    }
  }

  def aquery(targets: List[String]): Try[ActionGraphContainer] = {
    ProgressConsole.auto("Querying action graph") { err =>
      val out = new ByteArrayOutputStream
      val cmd = List(bazel, "aquery", targets.mkString(" + ")) ++ List(
        "--noimplicit_deps",
        "--notool_deps",
        "--output=proto"
      )
      val code = run(cmd, out, err)
      if (code != 0) {
        throw new MessageOnlyException("Could not query action graph")
      }
      ActionGraphContainer.parseFrom(out.toByteArray())
    }
  }

  def targetsInfos(
      specs: List[String],
      supportedRules: List[String],
      forbiddenGenerators: List[String]
  ): Try[List[Target]] = {
    val queryStr =
      importableTargetsQuery(specs, supportedRules, forbiddenGenerators)
    query("Inspecting importable targets", queryStr).map(getTargets)
  }

  def dependenciesToBuild(
      specs: List[String],
      supportedRules: List[String],
      forbiddenGenerators: List[String]
  ): Try[List[String]] = {
    importDependencies(specs, supportedRules, forbiddenGenerators).map {
      targets =>
        targets.map { target =>
          val rule = target.getRule()
          val label = rule.getName()
          val ruleClass = rule.getRuleClass()
          if (
            label.startsWith(
              "//3rdparty/jvm/"
            ) && ruleClass == "third_party_jvm_import"
          ) {
            val pkg = label.stripPrefix("//").takeWhile(_ != ':')
            val name = label.substring(label.indexOf(':') + 1)
            s"@maven//:${pkg}_${name}"
          } else {
            label
          }
        }
    }
  }

  private def importDependencies(
      specs: List[String],
      supportedRules: List[String],
      forbiddenGenerators: List[String]
  ): Try[List[Target]] = {
    val importableSpecs =
      specs.map(spec =>
        importableTargetsQuery(spec :: Nil, supportedRules, forbiddenGenerators)
      )
    val thirdPartyDeps = specs
      .map(spec => s"""filter("@maven//:.+?[^j][^a][^r]$$", deps($spec))""")
      .mkString(" + ")
    val deps = importableSpecs.map(spec => s"""deps($spec)""").mkString(" + ")
    val subs = importableSpecs.mkString(" - ")
    val queryStr = s"kind(rule, $deps + $thirdPartyDeps - $subs)"
    query("Computing dependencies", queryStr).map(getTargets)
  }

  private def importableTargetsQuery(
      specs: List[String],
      supportedRules: List[String],
      forbiddenGenerators: List[String]
  ): String = {
    val targets = specs.mkString(" + ")
    val pattern = supportedRules.mkString("|")
    val baseQuery = s"""kind("^($pattern)", $targets)"""
    s"""($baseQuery - attr(generator_function, "${forbiddenGenerators.mkString(
      "|"
    )}", $baseQuery))"""
  }

  private def getTargets(result: QueryResult): List[Target] = {
    result
      .getTargetList()
      .asScala
      .toList
      .filterNot(_.getRule().getName().endsWith(".semanticdb"))
  }

  def sourcesGlobs(
      pkgs: Iterable[String]
  ): Map[String, (List[String], List[String])] = {
    // Extract source globs by groups of at most 100 packages to avoid
    // going over the command line max size.
    pkgs
      .sliding(100, 100)
      .foldLeft(Map.empty[String, (List[String], List[String])]) {
        case (acc, pkgGroup) =>
          acc ++ groupedSourcesGlobs(pkgGroup)
      }
  }

  private def groupedSourcesGlobs(
      pkgs: Iterable[String]
  ): Map[String, (List[String], List[String])] = {
    val out = new ByteArrayOutputStream
    val cmd = List("pants-support/buildifier/bin/buildozer-bazel") ++
      pkgs.map(_ + ":all") ++
      List("print label kind sources")

    val code = run(cmd, out)
    val targetInfo = """(.+) (.+) \[(.+)\]""".r
    // 1 is the only exit code that indicates complete failure.
    if (code != 1) {
      val lines = out.toString("UTF-8").linesIterator
      lines.foldLeft(Map.empty[String, (List[String], List[String])]) {
        case (acc, line @ targetInfo(lbl, _, pats)) =>
          val (excludes, includes) =
            pats.split(" ").toList.partition(_.startsWith("!"))
          acc + (lbl -> ((includes, excludes.map(_.tail))))
        case (acc, line) =>
          acc
      }
    } else {
      Map.empty
    }
  }

  def run(
      cmd: List[String],
      out: OutputStream,
      err: OutputStream = FileUtils.NullOutputStream
  ): Int = {
    val io =
      new ProcessIO(_ => (), FileUtils.copy(_, out), FileUtils.copy(_, err))
    Process(cmd, cwd.toFile)
      .run(io)
      .exitValue()
  }

}
