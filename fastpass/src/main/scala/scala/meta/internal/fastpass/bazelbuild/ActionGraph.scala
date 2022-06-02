package scala.meta.internal.fastpass.bazelbuild

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashSet

import scala.meta.internal.fastpass.bazelbuild.AnalysisProtosV2._

import ujson.Obj
import ujson.Str
import ujson.Value

class ActionGraph(
    labelToActions: Map[String, List[Action]],
    idToDepSetOfFiles: Map[Int, DepSetOfFiles],
    idToArtifact: Map[Int, Artifact],
    idToPathFragment: Map[Int, PathFragment]
) {

  def outputsOf(label: String): List[Artifact] = {
    labelToActions
      .getOrElse(label, Nil)
      .flatMap(_.getOutputIdsList().asScala)
      .flatMap(x => idToArtifact.get(x))
  }

  def transitiveInputsOf(
      label: String,
      mnemonics: Set[String]
  ): List[Artifact] = {
    for {
      action <- labelToActions.getOrElse(label, Nil)
      if mnemonics.contains(action.getMnemonic())
      input <- transitiveInputsOf(action)
    } yield input
  }

  def transitiveInputsOf(action: Action): List[Artifact] = {
    val depSets = new java.util.LinkedList(action.getInputDepSetIdsList())

    val buffer = Buffer.empty[Artifact]
    val visited = HashSet.empty[Int]

    while (!depSets.isEmpty) {
      val depSetId = depSets.removeFirst()
      if (visited.add(depSetId)) {
        idToDepSetOfFiles.get(depSetId).foreach { depSet =>
          depSet.getDirectArtifactIdsList().forEach { artifactId =>
            idToArtifact.get(artifactId).foreach(buffer.append(_))
          }
          depSets.addAll(depSet.getTransitiveDepSetIdsList())
        }
      }
    }
    buffer.toList
  }

  def pathOf(artifact: Artifact): String =
    idToPathFragment.get(artifact.getPathFragmentId()) match {
      case None => ""
      case Some(fragment) => pathOf(fragment)
    }

  private def pathOf(fragment: PathFragment): String = {
    idToPathFragment.get(fragment.getParentId()) match {
      case None => fragment.getLabel()
      case Some(parent) => pathOf(parent) + "/" + fragment.getLabel()
    }
  }

  def toJson: Obj = {
    val newJson = Obj()
    newJson("labelToActions") = JsonUtils.mapToJson(labelToActions)(
      "label",
      Str(_),
      "actions",
      _.map(JsonUtils.protoToJson)
    )
    newJson("idToDepSetOfFiles") = JsonUtils.mapToJson(idToDepSetOfFiles)(
      "id",
      _.toString,
      "depset",
      JsonUtils.protoToJson
    )
    newJson("idToArtifact") = JsonUtils.mapToJson(idToArtifact)(
      "id",
      _.toString,
      "artifact",
      JsonUtils.protoToJson
    )
    newJson("idToPathFragment") = JsonUtils.mapToJson(idToPathFragment)(
      "id",
      _.toString,
      "pathFragment",
      JsonUtils.protoToJson
    )
    newJson
  }

}

object ActionGraph {
  def apply(actionGraph: ActionGraphContainer): ActionGraph = {
    val labelToActions = ActionGraph.labelToActions(actionGraph)
    val idToDepSetOfFiles = ActionGraph.idToDepSetOfFiles(actionGraph)
    val idToArtifact = ActionGraph.idToArtifact(actionGraph)
    val idToPathFragment = ActionGraph.idToPathFragment(actionGraph)
    new ActionGraph(
      labelToActions,
      idToDepSetOfFiles,
      idToArtifact,
      idToPathFragment
    )
  }

  def fromJson(js: Value): ActionGraph = {
    val labelToAction = JsonUtils.mapFromJson(
      js("labelToActions"),
      "label",
      _.str,
      "actions",
      _.arr.toList.map(JsonUtils.jsonToProto(_)(Action.parseFrom))
    )
    val idToDepSetOfFiles = JsonUtils.mapFromJson(
      js("idToDepSetOfFiles"),
      "id",
      _.str.toInt,
      "depset",
      JsonUtils.jsonToProto(_)(DepSetOfFiles.parseFrom)
    )
    val idToArtifact = JsonUtils.mapFromJson(
      js("idToArtifact"),
      "id",
      _.str.toInt,
      "artifact",
      JsonUtils.jsonToProto(_)(Artifact.parseFrom)
    )
    val idToPathFragment = JsonUtils.mapFromJson(
      js("idToPathFragment"),
      "id",
      _.str.toInt,
      "pathFragment",
      JsonUtils.jsonToProto(_)(PathFragment.parseFrom)
    )
    new ActionGraph(
      labelToAction,
      idToDepSetOfFiles,
      idToArtifact,
      idToPathFragment
    )
  }

  private def idToPathFragment(
      actionGraph: ActionGraphContainer
  ): Map[Int, PathFragment] = {
    actionGraph
      .getPathFragmentsList()
      .asScala
      .toList
      .foldLeft(Map.empty[Int, PathFragment]) {
        case (acc, fragment) =>
          acc + (fragment.getId -> fragment)
      }
  }

  private def labelToActions(
      actionGraph: ActionGraphContainer
  ): Map[String, List[Action]] = {
    val idToLabel =
      actionGraph.getTargetsList().asScala.foldLeft(Map.empty[Int, String]) {
        case (acc, tgt) =>
          acc + (tgt.getId() -> tgt.getLabel())
      }
    actionGraph
      .getActionsList()
      .asScala
      .toList
      .groupBy(a => idToLabel(a.getTargetId()))
  }

  private def idToDepSetOfFiles(
      actionGraph: ActionGraphContainer
  ): Map[Int, DepSetOfFiles] = {
    actionGraph
      .getDepSetOfFilesList()
      .asScala
      .toList
      .foldLeft(Map.empty[Int, DepSetOfFiles]) {
        case (acc, depSet) =>
          acc + (depSet.getId() -> depSet)
      }
  }

  private def idToArtifact(
      actionGraph: ActionGraphContainer
  ): Map[Int, Artifact] = {
    actionGraph
      .getArtifactsList()
      .asScala
      .toList
      .foldLeft(Map.empty[Int, Artifact]) {
        case (acc, artifact) =>
          acc + (artifact.getId() -> artifact)
      }
  }
}
