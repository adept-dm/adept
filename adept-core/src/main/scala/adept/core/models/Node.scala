package adept.core.models

import adept.core.Adept

private[core] sealed class NodeLike[T <: NodeLike[T]](//need parameter to avoid cyclic references to NodeLike
    val module: Module,
    val configurations: scala.collection.Set[Configuration],
    val artifacts: scala.collection.Set[Artifact], 
    val evictedArtifacts: scala.collection.Set[EvictedArtifact], 
    val children: scala.collection.Set[T], 
    val evictedModules: scala.collection.Set[EvictedModule],
    val missingDependencies: Set[MissingDependency]) //you cannot whether a dependency is missing or not so it is always a immutable Set

case class Node(override val module: Module, override val configurations: Set[Configuration], override val artifacts: Set[Artifact], override val evictedArtifacts: Set[EvictedArtifact], override val children: Set[Node], override val evictedModules: Set[EvictedModule], override val missingDependencies: Set[MissingDependency]) extends NodeLike[Node](module, configurations, artifacts, evictedArtifacts, children, evictedModules, missingDependencies) { //TODO: is there a way too achieve Mutable and Immutable Nodes wihtout being clever with Parameters? Want code to be easy to understand for Java people
  def asMutable: MutableNode = {
    import collection.{Set => _, _}
    val deps: mutable.Set[MutableNode] = children.map(_.asMutable)(breakOut)
    MutableNode(module,
      //TODO: is this the best way to convert to mutable.Set?
      mutable.Set.empty ++ configurations, 
      mutable.Set.empty ++ artifacts,
      mutable.Set.empty ++ evictedArtifacts,
      deps,
      mutable.Set.empty ++ evictedModules,
      missingDependencies
    )
  }
}

private[core] case class MutableNode(override val module: Module, override val configurations: collection.mutable.Set[Configuration], override val artifacts: collection.mutable.Set[Artifact], override val evictedArtifacts: collection.mutable.Set[EvictedArtifact], override val children: collection.mutable.Set[MutableNode], override val evictedModules: collection.mutable.Set[EvictedModule], override val missingDependencies: Set[MissingDependency]) extends NodeLike[MutableNode](module, configurations, artifacts, evictedArtifacts, children, evictedModules, missingDependencies) { 
  def asImmutable: Node = {
    Node(module,
      //TODO: is this the best way to convert to immutable.Set?
      configurations.toSet, 
      artifacts.toSet,
      evictedArtifacts.toSet,
      children.toSet[MutableNode].map(_.asImmutable),
      evictedModules.toSet,
      missingDependencies
    )
  }
  
  //TODO: move logic out of models
  def evict(module: Module, reason: String): Unit = synchronized {
    children.find(n => n.module == module).foreach{ node =>
      children -= node 
      evictedModules += EvictedModule(module, reason)
    }
  }
  
  def evict(artifact: Artifact, reason: String) = synchronized {
    artifacts.find(_ == artifact).foreach{ artifact =>
      artifacts -= artifact
      evictedArtifacts += EvictedArtifact(artifact, reason)
    }
  }
}
