package scala.meta.internal.fastpass.bazelbuild

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStreamReader
import java.io.OutputStream
import java.io.PrintStream
import java.io.PrintWriter
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.sys.process.Process
import scala.sys.process.ProcessIO
import scala.util.Failure
import scala.util.Try

import scala.meta.internal.fastpass.FileUtils
import scala.meta.internal.fastpass.MessageOnlyException
import scala.meta.internal.fastpass.bazelbuild.AnalysisProtosV2.ActionGraphContainer
import scala.meta.internal.fastpass.bazelbuild.Build.Attribute
import scala.meta.internal.fastpass.bazelbuild.Build.QueryResult
import scala.meta.internal.fastpass.bazelbuild.Build.Target
import scala.meta.internal.fastpass.console.ProgressConsole

import com.google.protobuf.TextFormat

object Bazel {
  def isPlainSpec(spec: String): Boolean =
    !spec.contains("(") && !spec.contains(" ")

  def getAttribute(
      target: Target,
      attribute: String
  ): Option[Attribute] =
    target.getRule().getAttributeList().asScala.find(_.getName() == attribute)

  private val extToExclude = List(
    ".semanticdb",
    ".deployjar",
    ".dirbundle",
    ".dataconfig",
    ".dataconfigjar"
  )

  private object Alias {
    def unapply(target: Target): Option[List[String]] =
      target.getRule().getRuleClass() match {
        case "alias" =>
          getAttribute(target, "actual").map(_.getStringValue() :: Nil)
        case "target_union" =>
          getAttribute(target, "deps").map(
            _.getStringListValueList().asScala.toList
          )
        case _ =>
          None
      }
  }

}

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
    ProgressConsole.auto(title)(queryRaw(expr, _)).recoverWith {
      case _ => Failure(new MessageOnlyException(title + " failed"))
    }
  }

  private def queryRaw(expr: String, err: PrintStream): QueryResult = {
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
      throw new MessageOnlyException("Non-zero exit code")
    } else {
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
        // Use textproto instead of proto, see https://github.com/bazelbuild/bazel/issues/12984
        "--output=textproto"
      )
      val code = run(cmd, out, err)
      if (code != 0) {
        throw new MessageOnlyException("Could not query action graph")
      }
      val in =
        new InputStreamReader(new ByteArrayInputStream(out.toByteArray()))
      val builder = ActionGraphContainer.newBuilder()
      TextFormat.getParser().merge(in, builder)
      builder.build()
    }
  }

  def targetsInfos(
      specs: List[String],
      supportedRules: List[String],
      forbiddenGenerators: Map[String, List[String]]
  ): Try[List[Target]] = {
    val queryStr =
      importableTargetsQuery(specs, supportedRules, forbiddenGenerators)
    query("Inspecting importable targets", queryStr)
      .map(getTargets)
      .flatMap(dealiasTargets)
  }

  def dependenciesToBuild(
      specs: List[String],
      supportedRules: List[String],
      forbiddenGenerators: Map[String, List[String]]
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
      forbiddenGenerators: Map[String, List[String]]
  ): Try[List[Target]] = {
    val importableSpecs =
      specs
        .map(spec =>
          importableTargetsQuery(
            spec :: Nil,
            supportedRules,
            forbiddenGenerators
          )
        )
        .mkString(" + ")
    val queryStr = s"""let importableSpecs = $importableSpecs in
                      |let dependencies = deps($$importableSpecs) in
                      |let windowsOnly = attr(tags, "jvm_classifier=windows", $$dependencies) in
                      |kind(rule, $$dependencies - $$windowsOnly - $$importableSpecs)""".stripMargin
    query("Computing dependencies", queryStr).map(getTargets)
  }

  private def importableTargetsQuery(
      specs: List[String],
      supportedRules: List[String],
      forbiddenGenerators: Map[String, List[String]]
  ): String = {
    val targets = specs.mkString(" + ")
    val pattern = supportedRules.mkString("|")
    val baseQuery = s"""kind("^($pattern)", $targets)"""
    val excludedGenerators = forbiddenGenerators.foldLeft(List.empty[String]) {
      case (acc, ("", generators)) =>
        s"""attr(generator_function, "${generators.mkString(
          "|"
        )}", $$baseQuery)""" :: acc
      case (acc, (rule, generators)) =>
        s"""attr(generator_function, "${generators.mkString(
          "|"
        )}", kind($rule, $$baseQuery))""".stripMargin :: acc
    }

    excludedGenerators match {
      case Nil =>
        baseQuery
      case generators =>
        s"""let baseQuery = $baseQuery in
           |($$baseQuery - ${generators.mkString(" - ")})""".stripMargin
    }
  }

  private def filterIgnoredTargets(targets: List[Target]): List[Target] = {
    targets.filterNot(t =>
      Bazel.extToExclude.exists(e => t.getRule().getName().endsWith(e))
    )
  }

  private def getTargets(result: QueryResult): List[Target] = {
    filterIgnoredTargets(result.getTargetList().asScala.toList)
  }

  private def dealiasTargets(targets: List[Target]): Try[List[Target]] = {
    ProgressConsole.manual("Dealias targets") { advance =>
      def partitionAliases(
          targets: List[Target]
      ): (List[Target], List[Target]) =
        filterIgnoredTargets(targets)
          .partition { target =>
            val rule = target.getRule().getRuleClass()
            rule == "alias" || rule == "target_union"
          }

      def groupByName(targets: List[Target]): Map[String, Target] =
        targets.map(t => t.getRule.getName -> t).toMap

      @scala.annotation.tailrec
      def inner(
          plainTargets: Map[String, Target],
          resolvedAliases: Set[String],
          newTargets: List[Target]
      ): List[Target] = {
        advance(plainTargets.size, plainTargets.size + newTargets.size)
        val (newAliases, newPlainTargets) = partitionAliases(newTargets)
        val newResolvedAliases =
          resolvedAliases ++ newAliases.map(_.getRule.getName)
        val allPlainTargets = plainTargets ++ groupByName(newPlainTargets)

        val toQuery = newAliases
          .collect {
            case Bazel.Alias(aliased) =>
              aliased.filterNot(allPlainTargets.contains)
          }
          .flatten
          .filterNot(newResolvedAliases.contains)

        if (toQuery.isEmpty) allPlainTargets.values.toList
        else {
          val newTargets = queryRaw(
            toQuery.mkString(" + "),
            FileUtils.NullPrintStream
          ).getTargetList().asScala.toList
          inner(allPlainTargets, newResolvedAliases, newTargets)
        }
      }

      inner(Map.empty, Set.empty, targets)
    }
  }

  def sourcesGlobs(
      pkgs: Iterable[String]
  ): Try[Map[String, (List[String], List[String])]] = {
    ProgressConsole.manual("Inspecting targets source globs") { advance =>
      val total = pkgs.size

      // Extract source globs by groups of at most 100 packages to avoid
      // going over the command line max size.
      pkgs
        .sliding(100, 100)
        .foldLeft(Map.empty[String, (List[String], List[String])]) {
          case (acc, pkgGroup) =>
            val newAcc = acc ++ groupedSourcesGlobs(pkgGroup)
            advance(pkgGroup.size, total)
            newAcc
        }
    }
  }

  private def groupedSourcesGlobs(
      pkgs: Iterable[String]
  ): Map[String, (List[String], List[String])] = {
    val out = new ByteArrayOutputStream
    val cmd = List("pants-support/buildifier/bin/buildozer-bazel") ++
      pkgs.map(_ + ":all") ++
      List("print label sources")

    val code = run(cmd, out)
    val targetInfo = """(.+) \[(.+)\]""".r
    val noSourcesTargetInfo = """(.+) \(missing\)""".r
    // 1 is the only exit code that indicates complete failure.
    if (code != 1) {
      val lines = out.toString("UTF-8").linesIterator
      lines.foldLeft(Map.empty[String, (List[String], List[String])]) {
        case (acc, line @ noSourcesTargetInfo(lbl)) =>
          acc + (lbl -> ((Nil, Nil)))
        case (acc, line @ targetInfo(lbl, pats)) =>
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
    val errWriter = new PrintWriter(err)
    errWriter.println(s"# Running command in ${cwd}:")
    errWriter.println(s"$$ ${cmd.mkString(" ")}")
    Process(cmd, cwd.toFile)
      .run(io)
      .exitValue()
  }

}
