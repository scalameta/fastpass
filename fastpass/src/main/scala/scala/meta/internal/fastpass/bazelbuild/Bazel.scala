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

  def enclosingPackage(target: Target): String = {
    val label = target.getRule.getName
    val index = label.indexOf(':')
    if (index == -1) label else label.substring(0, index)
  }

  def isPlainSpec(spec: String): Boolean =
    !spec.contains("(") && !spec.contains(" ")

  def getAttribute(
      target: Target,
      attribute: String
  ): Option[Attribute] =
    target.getRule().getAttributeList().asScala.find(_.getName() == attribute)

  def generatorFunction(target: Target): Option[String] =
    getAttribute(target, "generator_function").map(_.getStringValue())

  val ScroogeWorkerLabel =
    "@io_bazel_rules_scala//src/scala/scripts:scrooge_worker"
  val ScroogeWorkerMnemonic = "TemplateExpand"

  val PlatformClasspathLabel =
    "@bazel_tools//tools/jdk:platformclasspath"

  val ProtobufJava =
    "@com_google_protobuf_protobuf_java//:com_google_protobuf_protobuf_java"
  val ProtobufJavaMnemonic =
    "JavaIjar"

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

  def aquery(
      targets: List[String],
      retries: Integer
  ): Try[ActionGraphContainer] = {
    val bazelInternalError = "FATAL: bazel crashed due to an internal error."

    ProgressConsole.auto("Querying action graph") { err =>
      val out = new ByteArrayOutputStream
      val cmd = List(bazel, "aquery", targets.mkString(" + ")) ++ List(
        "--noimplicit_deps",
        "--notool_deps",
        // Use textproto instead of proto, see https://github.com/bazelbuild/bazel/issues/12984
        "--output=textproto"
      )

      val code = {
        val errByteArrayOutputStream = new ByteArrayOutputStream
        val code = run(cmd, out, errByteArrayOutputStream)
        if (
          retries > 0 && errByteArrayOutputStream
            .toString()
            .contains(bazelInternalError)
        ) {
          aquery(targets, retries - 1)
        }
        err.write(errByteArrayOutputStream.toByteArray)
        code
      }

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
      forbiddenGenerators: Map[String, List[String]],
      forbiddenTags: List[String]
  ): Try[List[Target]] = {
    val queryStr =
      importableTargetsQuery(
        specs,
        supportedRules,
        forbiddenGenerators,
        forbiddenTags
      )
    query("Inspecting importable targets", queryStr)
      .map(getTargets)
      .flatMap(dealiasTargets)
  }

  def dependenciesToBuild(
      specs: List[String],
      supportedRules: List[String],
      forbiddenGenerators: Map[String, List[String]],
      forbiddenTags: List[String]
  ): Try[List[String]] = {
    importDependencies(
      specs,
      supportedRules,
      forbiddenGenerators,
      forbiddenTags
    ).map { targets =>
      targets.map { target =>
        val rule = target.getRule()
        val label = rule.getName()
        val ruleClass = rule.getRuleClass()
        // Replace `//3rdparty/jvm/...` with the actual target in the `maven` repository, so that
        // Bazel will build the source jars for these targets.
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
      forbiddenGenerators: Map[String, List[String]],
      forbiddenTags: List[String]
  ): Try[List[Target]] = {
    val importableSpecs =
      specs
        .map(spec =>
          importableTargetsQuery(
            spec :: Nil,
            supportedRules,
            forbiddenGenerators,
            forbiddenTags
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
      forbiddenGenerators: Map[String, List[String]],
      forbiddenTags: List[String]
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
    val excludedTags = s"""attr(tags, "${forbiddenTags.mkString(
      "|"
    )}", $$baseQuery)"""

    excludedGenerators match {
      case Nil =>
        s"""let baseQuery = $baseQuery in
           |($$baseQuery - ${excludedTags}""".stripMargin
      case generators =>
        s"""let baseQuery = $baseQuery in
           |($$baseQuery - ${generators.mkString(" - ")}
           | - ${excludedTags})""".stripMargin
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
  ): Try[Map[String, SourcesInfo]] = {
    ProgressConsole.manual("Inspecting targets source globs") { advance =>
      val total = pkgs.size

      // Extract source globs by groups of at most 100 packages to avoid
      // going over the command line max size.
      pkgs
        .sliding(100, 100)
        .foldLeft(Map.empty[String, SourcesInfo]) {
          case (acc, pkgGroup) =>
            val newAcc = acc ++ groupedSourcesGlobs(pkgGroup)
            advance(pkgGroup.size, total)
            newAcc
        }
    }
  }

  def groupedSourcesGlobs(
      pkgs: Iterable[String]
  ): Map[String, SourcesInfo] = {
    val out = new ByteArrayOutputStream
    val cmd = List("pants-support/buildifier/bin/buildozer-bazel") ++
      pkgs.map(_ + ":all") ++
      List("print label sources java_sources")

    val code = run(cmd, out)

    val sourcesAndJavaSources = """(.+) \[(.+)\] \[(.+)\]""".r
    val sourcesNoJavaSources = """(.+) \[(.+)\] \(missing\)""".r
    val noSourcesJavaSources = """(.+) \(missing\) \[(.+)\]""".r
    val noSourcesNoJavaSources = """(.+) \(missing\) \(missing\)""".r

    def preparePatterns(patterns: String): (List[String], List[String]) = {
      val (excludes, includes) =
        patterns.split(" ").toList.partition(_.startsWith("!"))
      (includes, excludes.map(_.tail))
    }

    def prepareJavaSources(javaSources: String): List[String] = {
      javaSources.split(" ").toList
    }

    // 1 is the only exit code that indicates complete failure.
    if (code != 1) {
      val lines = out.toString("UTF-8").linesIterator
      lines.foldLeft(Map.empty[String, SourcesInfo]) {
        case (acc, sourcesAndJavaSources(lbl, pats, javaSources)) =>
          val (includes, excludes) = preparePatterns(pats)
          acc + (lbl -> SourcesInfo(
            includes,
            excludes,
            prepareJavaSources(javaSources)
          ))

        case (acc, sourcesNoJavaSources(lbl, pats)) =>
          val (includes, excludes) = preparePatterns(pats)
          acc + (lbl -> SourcesInfo(includes, excludes, Nil))

        case (acc, noSourcesJavaSources(lbl, javaSources)) =>
          acc + (lbl -> SourcesInfo(Nil, Nil, prepareJavaSources(javaSources)))

        case (acc, noSourcesNoJavaSources(lbl)) =>
          acc + (lbl -> SourcesInfo(Nil, Nil, Nil))

        case (acc, _) =>
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
