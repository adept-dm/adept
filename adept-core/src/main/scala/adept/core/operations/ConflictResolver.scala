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

  /** evict modules that have lower versions */
  private def evictedModules(tree: MutableTree) = {
    val coords = tree.nodes

    val evictedNodes = coords.groupBy(n => key(n.module.coordinates))
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

  /** defines what "same" coordinates means */
  private def key(coords: Coordinates): (String, String) = coords.org -> coords.name
  private def key(descriptor: DependencyDescriptor): (String, String) = descriptor.organization -> descriptor.name
  private def key(node: MutableNode): (String, String) = key(node.module.coordinates)
  private def key(evictedModule: EvictedModule): (String, String) = key(evictedModule.module.coordinates)
  private def key(missingDependency: MissingDependency): (String, String) = key(missingDependency.descriptor)

  private case class OverrideData(descriptor: DependencyDescriptor, declaredBy: MutableNode)

  /** find non-evicted overrides in a node */
  private def nodeOverrides(node: MutableNode): Set[OverrideData] = {
    val nonEvictedDeps = for { //non-evicted dependencies
      dep <- node.module.dependencies if dep.force
      child <- node.children if dep.coordinates == child.module.coordinates && dep.uniqueId == Some(child.module.uniqueId)
    } yield {
      OverrideData(dep, node)
    }

    val evictedCoords = node.evictedModules.map(em => key(em.module.coordinates)) ++ node.missingDependencies.filter(_.evicted).map(d => key(d.descriptor.asCoordinates))
    val nonEvictedOverrides = for { //remove overrides that have been declared in this same module
      o <- node.module.overrides if !evictedCoords.contains(key(o.asCoordinates))
    } yield {
      OverrideData(o, node)
    }

    (nonEvictedOverrides ++ nonEvictedDeps)
  }

  private def populateOverrides(overrides: collection.mutable.Map[(String, String), OverrideData])(root: MutableNode)(include: DependencyDescriptor => Boolean): Unit = {
    def populateOverrides(node: MutableNode): Unit = {
      val current = nodeOverrides(node)
      val newOverrides = current.collect {
        //only take into account an override that has not been added earlier:
        case data if include(data.descriptor) =>
          key(data.descriptor) -> data
      }
      
      overrides ++= newOverrides //side-effect
      node.children.foreach(populateOverrides)
    }
    populateOverrides(root)
  }

  /** build and add new node to parent */
  private def buildNode(newNodes: collection.mutable.Set[MutableNode], overrides: collection.mutable.Map[(String, String), OverrideData], parent: MutableNode, descriptor: DependencyDescriptor)(configurationMapping: String => String, findModule: Adept.FindModule): collection.mutable.Map[(String, String), OverrideData] = {
    val newOverrides = new collection.mutable.HashMap[(String, String), OverrideData] with collection.mutable.SynchronizedMap[(String, String), OverrideData]

    findModule(descriptor.asCoordinates, descriptor.uniqueId, Set.empty[Universe]) match { //TODO: fix universes
      case Right(Some(module)) =>
        //FIXME: should exclusions be here?
        val node = TreeOperations.build(module, true, Set.empty, parent.configurations, configurationMapping, findModule)
          .copy(postBuildInsertReason = Some("inserted because of override"))
        logger.trace("built new overridden node: " + node.module.coordinates + " " + node.module.uniqueId)

        //only populate overrides that are new to this tree and not already in the old tree

        val allNodes = MutableTree.nodes(node)

        allNodes.foreach { node =>
          //add new overrides
          populateOverrides(newOverrides)(node) { descriptor =>
            val descriptorKey = key(descriptor)

            !newOverrides.isDefinedAt(descriptorKey) && !overrides.isDefinedAt(descriptorKey)
          }
        }
        newNodes ++= allNodes
        parent.children += node
      case Right(None) =>
        val evicted = {
          val descriptorKey = key(descriptor.asCoordinates)
          parent.evictedModules.map(key(_)).contains(descriptorKey) ||
            parent.missingDependencies.filter(_.evicted).map(key(_)).contains(descriptorKey)
        }
        parent.missingDependencies += MissingDependency(descriptor, parent.module.coordinates, evicted = evicted, reason = ("missing dependency for override or dependency: " + descriptor.asCoordinates))
      case Left(conflictModules) => throw new Exception("found more than 1 module for: " + descriptor + ": " + conflictModules.mkString(",")) //TODO: handle gracefully? (remember to remove all references to it in the code)
    }

    newOverrides
  }

  /** replace overridden nodes with the new ones */
  private def replaceNodes(nodes: collection.mutable.Set[MutableNode], nodeMap: collection.mutable.HashMap[Int, MutableNode], overrides: collection.mutable.Map[(String, String), OverrideData])(configurationMapping: String => String, findModule: Adept.FindModule) = {
    val newOverrides = new collection.mutable.HashMap[(String, String), OverrideData] with collection.mutable.SynchronizedMap[(String, String), OverrideData]
    val newNodes = new collection.mutable.HashSet[MutableNode] with collection.mutable.SynchronizedSet[MutableNode]

    def isSame(coords: Coordinates, maybeUniqueId: Option[UniqueId], overrideData: Option[OverrideData]): Boolean = {
      overrideData.map { d =>
        val sameUniqueId = (d.descriptor.uniqueId, maybeUniqueId) match {
          case (Some(uniqueId1), Some(uniqueId2)) => uniqueId1 == uniqueId2
          case (Some(uniqueId), None) => false //overridden descriptor specifies which id to use and none where found
          case (None, _) => true //overridden descriptor does not specify the id so always the same
        }

        d.descriptor.asCoordinates == coords && sameUniqueId
      }.getOrElse(false)
    }

    def replaceNodes(node: MutableNode): Unit = {
      val oldHash = node.hashCode

      nodeMap.get(oldHash) match {
        case Some(newNode) =>
          logger.trace("updating already detected node using previous found: " + node.module.coordinates + " " + node.module.uniqueId)
          
          //TODO: do circular dependency detection
          
        case None =>
          logger.trace("replacing overridden nodes in: " + node.module.coordinates + " " + node.module.uniqueId)

          val isOverridden = { child: MutableNode =>
            overrides.isDefinedAt(key(child)) && !isSame(child.module.coordinates, Some(child.module.uniqueId), overrides.get(key(child)))
          }

          //remove children:
          val overrideChildren = node.children.par.filter(isOverridden)
          overrideChildren.foreach { child =>
            val OverrideData(descriptor, declaredBy) = overrides(key(child))
            node.children -= child
            val reason = "overridden by: " + descriptor.asCoordinates + " in " + declaredBy.module.coordinates
            node.evictedModules += EvictedModule(child.module, reason)
            node.overriddenDependencies += OverriddenDependency(child.module.coordinates, declaredBy.module.coordinates, reason)
            val builtOverrides = buildNode(newNodes, overrides, node, descriptor)(configurationMapping, findModule)
            newOverrides ++= builtOverrides
          }

          //remove missing dependencies:
          val overrideDeps = node.missingDependencies.par.filter { missing =>
            overrides.isDefinedAt(key(missing)) && !isSame(missing.descriptor.asCoordinates, missing.descriptor.uniqueId, overrides.get(key(missing)))
          }
          overrideDeps.foreach { missing =>
            val OverrideData(descriptor, declaredBy) = overrides(key(missing))
            node.missingDependencies -= missing
            val reason = "overridden by: " + descriptor.asCoordinates + " in " + declaredBy.module.coordinates
            node.missingDependencies += missing.copy(evicted = true, reason = reason)
            node.overriddenDependencies += OverriddenDependency(missing.descriptor.asCoordinates, declaredBy.module.coordinates, reason)
            newOverrides ++= buildNode(newNodes, overrides, node, descriptor)(configurationMapping, findModule)
          }

          nodeMap += oldHash -> node
      }
    }

    nodes.foreach(replaceNodes)
    newOverrides -> newNodes
  }

  /** find and replace overridden dependencies */
  private def overrideVersions(tree: MutableTree, configurationMapping: String => String, findModule: Adept.FindModule): Unit = {
    import collection._
    val nodes = mutable.Set.empty ++ tree.nodes
    val nodeMap = new mutable.HashMap[Int, MutableNode] with mutable.SynchronizedMap[Int, MutableNode]

    val overrides = new mutable.HashMap[(String, String), OverrideData] with mutable.SynchronizedMap[(String, String), OverrideData]

    //calculate current overrides:
    populateOverrides(overrides)(tree.root)(descriptor => !overrides.isDefinedAt(key(descriptor)))

    //adjust tree and find new overrides for newly built nodes:
    var (newOverrides, newNodes: mutable.HashSet[MutableNode]) = replaceNodes(nodes, nodeMap, overrides)(configurationMapping, findModule)
    
    //adjust tree with as long as we find new nodes to overrides:
    while (newOverrides.nonEmpty || newNodes.nonEmpty) {
      overrides ++= newOverrides
      val tuple = replaceNodes(newNodes, nodeMap, overrides)(configurationMapping, findModule)
      newOverrides = tuple._1
      newNodes = tuple._2
    }
    
    
  }

  def resolveConflicts(tree: MutableTree, configurationMapping: String => String, findModule: Adept.FindModule): Unit = {
    //TODO: it might perform better if we perform overrides and evications in one pass?
    overrideVersions(tree, configurationMapping, findModule)
    evictedModules(tree)
  }
}
