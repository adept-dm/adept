package adept.core.models

import scala.collection.parallel.immutable.ParSet

private[core] sealed class TreeLike[N <: NodeLike[N]](confExpr: String, root: NodeLike[N]) {
  override def toString = {
    val indentSize = 2
    def artifactString(artifact: Artifact, indent: Int) = {
      (" " * indent) + "V___ " + artifact.hash + " " + artifact.configurations.mkString("(", ",", ")")
    }
    def evictedArtifactString(evictedArtifact: EvictedArtifact, indent: Int) = {
      (" " * indent) + "X___ " + evictedArtifact.artifact.hash + " (evicted: " + evictedArtifact.reason + ")"
    }

    def evictedModuleString(evictedModule: EvictedModule, indent: Int) = {
      (" " * indent) + "X___ " + evictedModule.module.coordinates + " (evicted: " + evictedModule.reason + ")"
    }

    def missingDependencyString(missingDependency: MissingDependency, indent: Int) = {
      (" " * indent) + "!___ " + missingDependency.descriptor.asCoordinates + " (missing: " + missingDependency.reason + ")"
    }

    def missingEvictedDependencyString(missingDependency: MissingDependency, indent: Int) = {
      (" " * indent) + "X___ " + missingDependency.descriptor.asCoordinates + " (evicted+missing: " + missingDependency.reason + ")"
    }

    def overriddenDependencyString(overriddenDependency: OverriddenDependency, indent: Int) = {
      (" " * indent) + "@___ " + overriddenDependency.descriptor.asCoordinates + " (overridden: " + overriddenDependency.reason + ")"
    }

    def childrenString(node: NodeLike[N], indent: Int): String = { //TODO: tailrec
      (" " * indent) + "V___ " + nodeString(node, indent + indentSize)
    }

    def nodeString(n: NodeLike[N], indent: Int) = {
      val (evictedMissing, missing) = n.missingDependencies.partition(_.evicted)

      n.module.coordinates + " " + n.configurations.map(_.name).mkString("(", ",", ")") + n.postBuildInsertReason.map(r => "[" + r + "]").getOrElse("") +
        (if (n.artifacts.nonEmpty || n.evictedArtifacts.nonEmpty) "\n" + (" " * indent) + "\\___ artifacts" else "") +
        (if (n.artifacts.nonEmpty) "\n" + n.artifacts.map(artifactString(_, indent + indentSize * 2)).mkString("", "\n", "") else "") +
        (if (n.evictedArtifacts.nonEmpty) "\n" + n.evictedArtifacts.map(evictedArtifactString(_, indent + indentSize * 2)).mkString("", "\n", "") else "") +
        (if (missing.nonEmpty) "\n" + (" " * indent) + "\\___ missing dependencies" else "") +
        (if (missing.nonEmpty) "\n" + missing.map(missingDependencyString(_, indent + indentSize * 2)).mkString("", "\n", "") else "") +
        (if (n.overriddenDependencies.nonEmpty) "\n" + (" " * indent) + "\\___ overridden dependencies" else "") +
        (if (n.overriddenDependencies.nonEmpty) "\n" + n.overriddenDependencies.map(overriddenDependencyString(_, indent + indentSize * 2)).mkString("", "\n", "") else "") +
        (if (n.children.nonEmpty) "\n" + n.children.map(childrenString(_, indent)).mkString("", "\n", "") else "") +
        (if (n.evictedModules.nonEmpty) "\n" + n.evictedModules.map(evictedModuleString(_, indent)).mkString("", "\n", "") else "") +
        (if (evictedMissing.nonEmpty) "\n" + evictedMissing.map(missingEvictedDependencyString(_, indent + indentSize * 2)).mkString("", "\n", "") else "")
    }

    nodeString(root, indentSize)
  }

}

private[core] case class MutableTree(confExpr: String, root: MutableNode) extends TreeLike(confExpr, root) {
  def toTree = {
    Tree(confExpr, root.asImmutable)
  }

  //TODO: figure out how to get the signatures right on nodes, overrides, ... if this is in TreeLike
  def nodes: Set[MutableNode] = {
    MutableTree.nodes(root)
  }

  def missing: Set[MissingDependency] = {
    def missing(node: MutableNode): Set[MissingDependency] = { //TODO: @tailrec?
      node.children.flatMap(missing).toSet ++ node.missingDependencies
    }
    missing(root)
  }

  def overrides: Set[(DependencyDescriptor, MutableNode)] = {
    MutableTree.overrides(root)
  }
}

private[core] object MutableTree {
  def nodes(node: MutableNode): Set[MutableNode] = { //TODO: @tailrec?
    node.children.flatMap(nodes).toSet + node
  }

  def overrides(node: MutableNode): Set[(DependencyDescriptor, MutableNode)] = { //TODO: @tailrec?
    def key(coords: Coordinates) = coords.org -> coords.name //TODO: use key everywhere org, name is referenced...
    val nonEvictedDeps = for { //non-evicted dependencies
      dep <- node.module.dependencies if dep.force
      child <- node.children if dep.coordinates == child.module.coordinates && dep.uniqueId == Some(child.module.uniqueId)
    } yield {
      (dep, node)
    }

    val evictedCoords = node.evictedModules.map(em => key(em.module.coordinates)) ++ node.missingDependencies.filter(_.evicted).map(d => key(d.descriptor.asCoordinates))
    val nonEvictedOverrides = for { //remove overrides that have been declared in this same module
      o <- node.module.overrides if !evictedCoords.contains(key(o.asCoordinates))
    } yield {
      (o, node)
    }
    node.children.flatMap(overrides).toSet ++ nonEvictedOverrides ++ nonEvictedDeps
  }
}

case class Tree(confExpr: String, root: Node) extends TreeLike(confExpr, root) {
  private[core] def toMutableTree = {
    MutableTree(confExpr, root.asMutable)
  }

  //TODO: figure out how to get the signatures right on nodes, overrides, ... if this is in TreeLike
  def artifacts: Set[Artifact] = {
    def artifacts(node: Node): ParSet[Artifact] = { //TODO: @tailrec?
      node.children.par.flatMap(artifacts(_)) ++ node.artifacts
    }
    artifacts(root).seq
  }

  def requiredMissing: Set[MissingDependency] = {
    def missing(node: Node): ParSet[MissingDependency] = { //TODO: @tailrec?
      node.children.par.flatMap(missing).toSet ++ node.missingDependencies.filter(!_.evicted)
    }
    missing(root).seq
  }
}