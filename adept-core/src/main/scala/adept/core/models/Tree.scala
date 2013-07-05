package adept.core.models

private[core] sealed class TreeLike[N <: NodeLike[N]](confExpr: String, root: NodeLike[N]) {
  override def toString = {
    val indentSize = 2
    def artifactString(artifact: Artifact, indent: Int) = {
      (" " * indent) + "V___ " + artifact.hash + " " + artifact.configurations.mkString("(", "," ,")")
    }
    def evictedArtifactString(evictedArtifact: EvictedArtifact, indent: Int) = {
      (" " * indent) + "X___ " + evictedArtifact.artifact.hash + " (evicted: " + evictedArtifact.reason + ")"
    }
    
    def evictedModuleString(evictedModule: EvictedModule, indent: Int) = {
      (" " * indent) + "X___ " + evictedModule.module.coordinates + " (evicted: " + evictedModule.reason + ")"
    }
    
    def missingDependencyString(missingDependency: MissingDependency, indent: Int) = {
      (" " * indent) + "!___ " + missingDependency.dependency.coordinates + " (missing: " + missingDependency.reason + ")"
    }
    
    def childrenString(node: NodeLike[N], indent: Int): String = { //TODO: tailrec
      (" " * indent) + "V___ " + nodeString(node, indent + indentSize)
    }

    def nodeString(n: NodeLike[N], indent: Int) = {
      n.module.coordinates + " "  + n.configurations.map(_.name).mkString("(", ",", ")") +
      (if (n.artifacts.nonEmpty || n.evictedArtifacts.nonEmpty) "\n" + (" " * indent) + "\\___ artifacts"  else "") +
      (if (n.artifacts.nonEmpty) "\n" + n.artifacts.map(artifactString(_, indent + indentSize*2)).mkString("", "\n", "") else "") +
      (if (n.evictedArtifacts.nonEmpty) "\n" + n.evictedArtifacts.map(evictedArtifactString(_, indent + indentSize*2)).mkString("", "\n", "") else "") +
      (if (n.missingDependencies.nonEmpty) "\n" + (" " * indent) + "\\___ missing dependencies"  else "") +
      (if (n.missingDependencies.nonEmpty) "\n" + n.missingDependencies.map(missingDependencyString(_, indent + indentSize*2)).mkString("", "\n", "") else "") +
      (if (n.children.nonEmpty) "\n" + n.children.map(childrenString(_, indent)).mkString("", "\n", "") else "") + 
      (if (n.evictedModules.nonEmpty) "\n" + n.evictedModules.map(evictedModuleString(_, indent)).mkString("", "\n", "") else "")
    }
    
    nodeString(root,indentSize)
  }
}

private[core] case class MutableTree(confExpr: String, root: MutableNode) extends TreeLike(confExpr, root) {
  def toTree = {
    Tree(confExpr, root.asImmutable)
  }
}

case class Tree(confExpr: String, root: Node) extends TreeLike(confExpr, root) {
  private[core] def toMutableTree = {
    MutableTree(confExpr, root.asMutable)
  } 
}