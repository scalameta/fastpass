package scala.meta.internal.fastpass.generic

import java.nio.file.Files

import scala.util.Try

import scala.meta.internal.fastpass.FastpassEnrichments._
import scala.meta.internal.fastpass.pantsbuild.PantsConfiguration
import scala.meta.internal.fastpass.pantsbuild.commands.StrictDepsMode
import scala.meta.io.AbsolutePath

import metaconfig.Conf
import ujson.Bool
import ujson.Str

case class Project(
    common: SharedOptions,
    name: String,
    targets: List[String],
    root: ProjectRoot,
    sources: SourcesMode,
    strictDeps: StrictDepsMode,
    importMode: ImportMode
) {
  val fuzzyName: String = PantsConfiguration.outputFilename(name)
  def matchesName(query: String): Boolean =
    Project.matchesFuzzyName(query, name, fuzzyName) ||
      targets == List(query)
  def bspRoot: AbsolutePath = root.bspRoot

  def matchesCreateOptions(
      options: CreateOptions
  ): Boolean = {
    val optionsName =
      PantsConfiguration.outputFilename(options.name.getOrElse(name))
    val optionsIsPants = options.pants
    val existingIsPants = importMode == ImportMode.Pants
    targets.sorted == options.targets.sorted &&
    optionsIsPants == existingIsPants &&
    optionsName == name
  }

}

object Project {
  def create(
      name: String,
      common: SharedOptions,
      targets: List[String],
      sources: SourcesMode,
      strictDeps: StrictDepsMode,
      importMode: ImportMode
  ): Project = {
    Project(
      common,
      name,
      targets,
      ProjectRoot(common.home.resolve(name)),
      sources,
      strictDeps,
      importMode
    )
  }
  def names(common: SharedOptions): List[String] =
    fromCommon(common).map(_.name)

  def matchesFuzzyName(
      query: String,
      projectName: String,
      fuzzyProjectName: String
  ): Boolean =
    projectName == query ||
      fuzzyProjectName == query

  def fromName(
      name: String,
      common: SharedOptions
  ): Option[Project] = {
    val fuzzyName = PantsConfiguration.outputFilename(name)
    fromCommon(
      common,
      { candidate =>
        matchesFuzzyName(candidate, name, fuzzyName)
      }
    ).headOption
  }
  def fromCommon(
      common: SharedOptions,
      isEnabled: String => Boolean = _ => true
  ): List[Project] = {
    for {
      project <- common.home.list.toBuffer[AbsolutePath].toList
      if isEnabled(project.filename)
      root = ProjectRoot(project)
      if root.bspJson.isFile
      json <- Try(ujson.read(root.bspJson.readText)).toOption
      targets <- json.obj.get("pantsTargets")
    } yield {
      val sources: SourcesMode = json.obj
        .get("sources")
        .collect {
          case Bool(bool) => Conf.Bool(bool)
          case Str(str) => Conf.Str(str)
        }
        .flatMap(c => SourcesMode.decoder.read(c).toEither.toOption)
        .getOrElse(SourcesMode.Default)
      val strictDeps: StrictDepsMode = json.obj
        .get("strictDeps")
        .collect {
          case Str(str) => Conf.Str(str)
        }
        .flatMap(c => StrictDepsMode.decoder.read(c).toEither.toOption)
        .getOrElse(StrictDepsMode.Default)
      val importMode: ImportMode = json.obj
        .get("importMode")
        .collect {
          case Str(ImportMode.Bazel.name) => ImportMode.Bazel
          case _ => ImportMode.Pants
        }
        .getOrElse(ImportMode.Pants)
      Project(
        common,
        project.filename,
        targets.arr.iterator.map(_.str).filter(_.nonEmpty).toList,
        root,
        sources,
        strictDeps,
        importMode
      )
    }
  }
  def current(common: SharedOptions): Option[Project] = {
    val workspaceBloop = common.bloopDirectory
    if (!Files.isSymbolicLink(workspaceBloop)) {
      None
    } else {
      val target = Files.readSymbolicLink(workspaceBloop)
      fromCommon(common).find(_.root.bloopRoot.toNIO == target)
    }
  }

}
