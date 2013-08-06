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

  private def findOverriddenNodes(nodes: Set[MutableNode], overrides: Set[(DependencyDescriptor, MutableNode)], configurationMapping: String => String, findModule: Adept.FindModule): Map[Coordinates, ((DependencyDescriptor, MutableNode, String), (MutableNode, Option[MutableNode], Option[MissingDependency]))] = {
    val allOverrides = overrides.par
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
          if (descriptor.name == "commons-logging") println("highest version: " + descriptor.asCoordinates)

          ((descriptor.organization, descriptor.name), (descriptor, node, ("overridden multiple places (" + allMostPopular.map { case (_, n) => n.module.coordinates }.mkString(",") + "). chose the one in: " + node.module.coordinates + " which is the most common override (" + numberOfPopular + ") sorted by highest versions.")))
        } else {
          comparableNodes.seq.headOption.map {
            case (descriptor, node) =>
              ((descriptor.organization, descriptor.name), (descriptor, node, ("overridden by " + descriptor.asCoordinates + " defined in: " + node.module.coordinates)))
          }.getOrElse {
            throw new Exception("found a key to override (" + key + "), but no nodes to do so")
          }
        }
    }.toMap

    def findDescriptors(node: MutableNode, oldCoords: Coordinates, child: Option[MutableNode], missing: Option[MissingDependency]) = {
      prunedOverrides.get(oldCoords.org -> oldCoords.name).flatMap {
        case (overrideDescriptor, parentNode, reason) =>
          if (overrideDescriptor.asCoordinates != oldCoords) { //do not override something which is the same
            Some((oldCoords), ((overrideDescriptor, parentNode, reason), (node, child, missing)))
          } else {
            None
          }
      }
    }
    //iterate over all nodes to find which nodes matches something to override in tree:
    val overriddenNodes = nodes.flatMap { node => //TODO: should be .par, but it might have synchronization issues??
      node.missingDependencies.flatMap { missing =>
        val oldCoords = missing.descriptor.asCoordinates
        findDescriptors(node, oldCoords, None, Some(missing))
      } ++ node.children.flatMap { child =>
        val oldCoords = child.module.coordinates
        findDescriptors(node, oldCoords, Some(child), None)
      }
    }.toMap

    //new overrides 
    val newOverrides = collection.mutable.Map.empty[Coordinates, ((DependencyDescriptor, MutableNode, String), (MutableNode, Option[MutableNode], Option[MissingDependency]))]

    //build tree based on overrides:
    for {
      (_, ((overrideDescriptor, parentNode, reason), (node, maybeChild, maybeMissing))) <- overriddenNodes
      if (!overriddenNodes.isDefinedAt(parentNode.module.coordinates)) //do not overwrite a module which is overridden itself
    } {
      val parent = parentNode.module.coordinates

      val oldCoords = (maybeChild, maybeMissing) match {
        case (Some(child), None) =>
          node.children -= child
          child.module.coordinates
        case (None, Some(missing)) =>
          node.missingDependencies -= missing
          missing.descriptor.asCoordinates
        case _ => throw new Exception("could not find child nor missing for: " + overrideDescriptor.asCoordinates + " in: " + parent)
      }
      
      findModule(overrideDescriptor.asCoordinates, overrideDescriptor.uniqueId, Set.empty) match { //TODO: fix universes
        case Right(Some(module)) =>
          val newNode = synchronized { //TODO: I think this is necessary, because you could end up in the same module?
            val root = TreeOperations.build(module, true, Set.empty, node.configurations, configurationMapping, findModule) //FIXME: should exclusions be here?
            //we have a new tree so we must include its new overrides
            newOverrides ++= findOverriddenNodes(Set(root), overrides ++ MutableTree.overrides(root), configurationMapping, findModule)
            println("new overrides: " + MutableTree.overrides(root) + " in " + root.module.coordinates)
            root
          }
          node.overriddenDependencies += OverriddenDependency(overrideDescriptor, parent, reason = ("overriding " + oldCoords + " because of " + overrideDescriptor.asCoordinates + overrideDescriptor.uniqueId.map(" id: " + _).getOrElse("") + " in " + parent))
          node.children += newNode.copy(postBuildInsertReason = Some("inserted because of override in: " + parent))
        case Right(None) =>
          node.overriddenDependencies += OverriddenDependency(overrideDescriptor, parent, reason = ("cannot apply because missing: " + overrideDescriptor.asCoordinates + overrideDescriptor.uniqueId.map(" id: " + _).getOrElse("") + " in " + parent))
          node.missingDependencies += MissingDependency(overrideDescriptor, parent, evicted = false, reason = "could not find dependency for override: " + overrideDescriptor.asCoordinates + " declared in: " + parent)
        case Left(conflictModules) => throw new Exception("found more than 1 module for: " + overrideDescriptor + ": " + conflictModules.mkString(",")) //TODO: handle gracefully? (remember to remove all references to it in the code)
      }
    }
    overriddenNodes ++ newOverrides
  }

  /** find dependencies that has to be overridden and replace them */
  private def overrideVersions(nodes: Set[MutableNode], overrides: Set[(DependencyDescriptor, MutableNode)], configurationMapping: String => String, findModule: Adept.FindModule): Unit = {
    //create the map which defines the overridden nodes:
    val currentOverriddenNodes = findOverriddenNodes(nodes, overrides, configurationMapping, findModule)
    //fix nodes that are overridden:
    for {
      (_, ((overrideDescriptor, parentNode, reason), (node, maybeChild, maybeMissing))) <- currentOverriddenNodes
      if (!currentOverriddenNodes.isDefinedAt(parentNode.module.coordinates)) //do not overwrite a module which is overridden itself
    } {
      println("replace: child:" + maybeChild.map(_.module.coordinates) + " or missing: " + maybeMissing.map(_.descriptor.asCoordinates) + " in: " + node.module.coordinates + " with " + overrideDescriptor.asCoordinates )
    }
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
    overrideVersions(tree.nodes, tree.overrides, configurationMapping, findModule)
    evictedModules(tree)
  }
}
