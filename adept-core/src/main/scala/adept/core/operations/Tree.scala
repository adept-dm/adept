package adept.core.operations

import adept.core.models._
import adept.utils.Logging

private[core] object Tree extends Logging {

  def evict(tree: MutableTree, module: Module, reason: String) = {
    def evict(node: MutableNode): Unit = { //TODO: @tailrec?
      node.evict(module, reason)
      node.children.foreach(evict)
    }
    evict(tree.root)
  }
  
  def build(confExpr: String, module: Module, configurationMapping: String => String, findModule: (Coordinates, Option[Hash]) => Option[Module]): MutableTree = {
    
    def build(parent: Module, parentExclusionRules: Set[DependencyExclusionRule], matchedConfigurations: collection.mutable.Set[Configuration]): MutableNode = {
      val extendedConfigurations = matchedConfigurations ++ matchedConfigurations.flatMap(c => ConfigurationResolver.extendedConfs(module.configurations, c) )
      val (moduleMatches, evictedModules, missingDependencies) = ConfigurationMatcher.matchingModules(parent.coordinates, parent.dependencies, parentExclusionRules, extendedConfigurations.toSet, configurationMapping, findModule)
      val (foundArtifacts, evictedArts) = ConfigurationMatcher.matchingArtifacts(parent.artifacts, extendedConfigurations.toSet)
      
      val dependentNodes = moduleMatches.par.map{ case (module, exclusionRules, confs) => //TODO: verify that par helps
        build(module, exclusionRules, confs)
      }
      MutableNode(module = parent, configurations = matchedConfigurations, artifacts = foundArtifacts, children = dependentNodes.seq, evictedArtifacts = evictedArts, evictedModules = evictedModules, missingDependencies = missingDependencies)
    }
    
    ConfigurationResolver.resolve(module.configurations, confExpr) match {
      case Right(confs) =>
        MutableTree(confExpr, build(module, Set.empty, confs)) 
      case Left(msg) =>
        logger.debug("no valid configurations found while building tree. " + msg)
        null //TODO: Either[String, ...]
        //Tree(confExpr, )
    }
  }
}