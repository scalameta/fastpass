package scala.meta.internal.fastpass.bazelbuild

import scala.meta.internal.fastpass.bazelbuild.Build.Target

class Extractors(importedTargets: List[Target]) {

  object ThriftRoot {
    private def thriftTarget(root: Target, language: String): Option[Target] = {
      val needleName =
        root.getRule.getName.stripSuffix("--thrift-root") + "-" + language
      importedTargets.find(_.getRule.getName == needleName)
    }
    def unapply(
        target: Target
    ): Option[(String, Option[Target], Option[Target])] = {
      val buildName = target.getRule.getName.stripSuffix("--thrift-root")
      if (target.getRule.getRuleClass() == "thrift_library") {
        val java = thriftTarget(target, "java")
        val scala = thriftTarget(target, "scala")
        if (java.isDefined || scala.isDefined) Some((buildName, java, scala))
        else None
      } else None
    }
  }

  object ThriftTarget {
    private def thriftTarget(
        target: Target,
        language: String
    ): Option[(Target, Target, String)] = {
      val needleName =
        target.getRule.getName.stripSuffix("-" + language) + "--thrift-root"
      importedTargets
        .find(_.getRule.getName == needleName)
        .map(root =>
          (target, root, if (language == "java") "Javac" else "Scalac")
        )
    }
    def unapply(target: Target): Option[(Target, Target, String)] =
      target.getRule.getRuleClass match {
        case "scrooge_scala_library" => thriftTarget(target, "scala")
        case "scrooge_java_library" => thriftTarget(target, "java")
        case _ => None
      }
  }

  object JvmAppTarget {
    def unapply(target: Target): Option[(Target, Target)] = {
      if (target.getRule().getRuleClass() != "_jvm_app") None
      else
        for {
          binaryLabel <- Bazel.getAttribute(target, "binary")
          binaryTarget <- scalaBinaryTargets.get(binaryLabel.getStringValue())
        } yield (target, binaryTarget)
    }
  }

  object ResourcesTarget {
    def unapply(target: Target): Boolean = {
      target.getRule().getRuleClass() == "scala_library" && Bazel
        .generatorFunction(
          target
        )
        .exists(_ == "resources")
    }
  }

  private val scalaBinaryTargets: Map[String, Target] = {
    importedTargets.collect {
      case target if target.getRule().getRuleClass() == "scala_binary" =>
        target.getRule().getName() -> target
    }.toMap
  }

  /**
   * Associates a Thrift root with the mnemonics and targets that implement it.
   */
  private val thriftTargets: Map[Target, Map[String, Target]] = {
    def update[K0, K1, V, U](
        m: Map[K0, Map[K1, V]],
        k0: K0,
        k1: K1,
        v: V
    ): Map[K0, Map[K1, V]] = {
      val newMapping = m.getOrElse(k0, Map.empty) + (k1 -> v)
      m + (k0 -> newMapping)
    }
    importedTargets.foldLeft(Map.empty[Target, Map[String, Target]]) {
      case (acc, ThriftTarget(target, thriftRoot, mnemonic)) =>
        update(acc, thriftRoot, mnemonic, target)
      case (acc, _) =>
        acc
    }
  }

}
