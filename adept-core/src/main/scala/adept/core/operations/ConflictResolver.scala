package adept.core.operations

import adept.core.models._
import adept.utils.Logging
import adept.core.Adept

private[core] object Version {
  val specialChars = "[\\._\\-\\+]"

  val SpecialMeanings = Map( //As DEFAULT_SPECIAL_MEANINGS in LatestVersionStrategy
    "dev" -> -1,
    "rc" -> 1,
    "final" -> 2)

  /**
   * Compares versions as in LatestVersionStrategy.MridComparator in Ivy.
   * This is again similar to the compare_version in PHP and works as described:
   * The function first replaces _, - and + with a dot . in the version strings and also inserts dots . before and after any non number so that for example '4.3.2RC1' becomes '4.3.2.RC.1'.
   * Then it splits the results on the dot.
   * Then it compares the parts starting from left to right.
   * If a part contains special version strings these are handled in the following order: dev < any string not found in this list < rc < final as in the val SpecialMeanings
   */
  def stringVersionCompare(v1: String, v2: String) = {

    def insertDotsAsArray(v: String) =
      v.replaceAll("([a-zA-Z])(\\d)", "$1.$2")
        .replaceAll("(\\d)([a-zA-Z])", "$1.$2").split(specialChars)

    def isNumber(s: String) = s.matches("\\d+")

    val parts1 = insertDotsAsArray(v1)
    val parts2 = insertDotsAsArray(v2)

    val differentParts = parts1.zip(parts2).dropWhile { case (v1, v2) => v1 == v2 }

    val maybeResult = differentParts.collectFirst {
      case (v1, v2) if (isNumber(v1) && !isNumber(v2)) => 1
      case (v1, v2) if (!isNumber(v1) && isNumber(v2)) => -1
      case (v1, v2) if (isNumber(v1) && isNumber(v2)) =>
        v1.toInt.compareTo(v2.toInt)
      case (v1, v2) if (SpecialMeanings.get(v1).isDefined || SpecialMeanings.get(v2).isDefined) =>
        (SpecialMeanings.get(v1), SpecialMeanings.get(v2)) match {
          case (Some(sm1), Some(sm2)) => {
            sm1.compareTo(sm2)
          }
          case (Some(sm1), None) => sm1.compareTo(0)
          case (None, Some(sm2)) => 0.compareTo(sm2)
          case somethingElse => throw new Exception("while comparing version got unexpected pair of versions: " + somethingElse)
        }
      case (v1, v2) => v1.compareTo(v2)
    }
    maybeResult.getOrElse {
      if (parts1.lastOption.isDefined && parts1.size > parts2.size) {
        if (isNumber(parts1.last)) 1
        else -1
      } else if (parts2.lastOption.isDefined && parts2.size > parts1.size) {
        if (isNumber(parts2.last)) 1
        else -1
      } else {
        0
      }
    }
  }
}

private[core] case class Version(private val value: String) extends Ordered[Version] {
  def compare(that: Version) = {
    Version.stringVersionCompare(this.value, that.value)
  }
}

private[core] object ConflictResolver extends Logging {

  /** find dependencies that has to be overridden and replace them */
  private def overrideVersions(tree: MutableTree, configurationMapping: String => String, findModule: Adept.FindModule): Unit = {
    val allOverrides = tree.overrides.par
      .groupBy { case (dependencyDescriptor, node) => dependencyDescriptor.organization -> dependencyDescriptor.name }

    //from all defined overrides or forced dependency, find which ones to use for this tree:
    val prunedOverrides = allOverrides.map {
      case (key, comparableNodes) =>
        if (comparableNodes.size > 1) {
          logger.debug("need consencus because found: " + comparableNodes.size + " nodes overriding " + key)

          //group by overridden versions (might be several that have the same):
          val groupedByVersion = comparableNodes.groupBy {
            case (descriptor, _) =>
              descriptor.preferredVersion
          }

          //take all the most popular version:
          val (_, mostPopularNodes) = groupedByVersion.maxBy { case (_, nodes) => nodes.size }
          val numberOfPopular = mostPopularNodes.size
          val allMostPopular = groupedByVersion.collect {
            case (_, nodes) if nodes.size == numberOfPopular =>
              nodes.seq
          }.toSet.flatten

          //take the highest of the popular versions:
          val (descriptor, node) = allMostPopular.maxBy { case (d, _) => Version(d.preferredVersion) }

          ((descriptor.organization, descriptor.name), (descriptor, node, ("overridden multiple places (" + allMostPopular.map { case (_, n) => n.module.coordinates }.mkString(",") + "). chose the on in: " + node.module.coordinates + " which is the most common override (" + numberOfPopular + ") sorted by highest versions.")))
        } else {
          comparableNodes.seq.headOption.map {
            case (descriptor, node) =>
              ((descriptor.organization, descriptor.name), (descriptor, node, ("overridden by " + descriptor.asCoordinates + " defined in: " + node.module.coordinates)))
          }.getOrElse {
            throw new Exception("found a key to override (" + key + "), but no nodes to do so")
          }
        }
    }.toMap

    //iterate over all nodes to find which nodes matches something to override in tree:
    val overriddenNodes = tree.nodes.par.flatMap { node =>
      node.children.flatMap { child =>
        val childCoords = child.module.coordinates
        prunedOverrides.get(childCoords.org -> childCoords.name).flatMap {
          case (descriptor, overrideNode, reason) =>
            if (descriptor.asCoordinates != childCoords) { //do not override something which is the same
              Some((child.module.coordinates), ((descriptor, overrideNode, reason), (node, child)))
            } else {
              None
            }
        }
      }
    }.seq.toMap

    //fix nodes that are overridden:
    for {
      (_, ((descriptor, overrideNode, reason), (node, child))) <- overriddenNodes
      if (!overriddenNodes.isDefinedAt(overrideNode.module.coordinates)) //do not overwrite a module which is overridden itself
    } {
      findModule(descriptor.asCoordinates, descriptor.uniqueId, Set.empty) match { //TODO: fix universes
        case Right(Some(module)) =>
          val newNode = TreeOperations.build(module, true, Set.empty, node.configurations, configurationMapping, findModule) //FIXME: should exclusions be here?
          val parent = overrideNode.module.coordinates
          //TODO: synchronize here? I do not _think_ it should be necessary because we are not touching the same node several times. Still it is scary to not do it
          node.children += newNode.copy(postBuildInsertReason = Some("inserted because of override in: " + parent))
          node.children -= child
          node.overriddenDependencies += OverriddenDependency(descriptor, child.module, parent, reason)
        case Right(None) =>
          val parent = node.module.coordinates
          node.children -= child
          node.overriddenDependencies += OverriddenDependency(descriptor, child.module, parent, reason)
          node.missingDependencies += MissingDependency(descriptor, parent, evicted = false, reason = "could not find dependency for override: " + descriptor.asCoordinates + " declared in: " + parent)
        case Left(conflictModules) => throw new Exception("found more than 1 module for: " + descriptor + ": " + conflictModules.mkString(",")) //TODO: handle gracefully? (remember to remove all references to it in the code)
      }
    }
    overriddenNodes
  }

  /** evict modules that have lower versions */
  private def evictedModules(tree: MutableTree) = {
    val coords = tree.nodes

    val evictedNodes = coords.groupBy(n => n.module.coordinates.org -> n.module.coordinates.name)
      .flatMap {
        case (_, comparableNodes) =>
          if (comparableNodes.size == 1) { //no need to evict if only one comparable node
            Set.empty[(MutableNode, String)]
          } else {
            val highestVersion = comparableNodes.maxBy(n => Version(n.module.coordinates.version))
            comparableNodes.filter(n => n.module.coordinates != highestVersion.module.coordinates).map(_ -> ("found higher version: " + highestVersion.module.coordinates))
          }
      }.seq.toSet

    TreeOperations.evict(tree,
      evictedNodes.map {
        case (node, reason) =>
          node.module -> reason
      })

  }

  def resolveConflicts(tree: MutableTree, configurationMapping: String => String, findModule: Adept.FindModule): Unit = {
    //TODO: it might perform better if we extract overrides and modules in one pass?
    overrideVersions(tree, configurationMapping, findModule)
    evictedModules(tree)
  }
}
