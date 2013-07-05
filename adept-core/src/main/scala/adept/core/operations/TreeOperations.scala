package adept.core.operations

import adept.core.models._
import adept.utils.Logging
import collection.{ Set=> _, _ }
import adept.core.Adept

private[core] object TreeOperations extends Logging {

  def evict(tree: MutableTree, module: Module, reason: String) = {
    def evict(node: MutableNode): Unit = { //TODO: @tailrec?
      node.evict(module, reason)
      node.children.foreach(evict)
    }
    evict(tree.root)
  }
  
  private def findNonTransitive(parent: Module, findModule: Adept.FindModule) = {
    val (evictedModuleOpts, missingDepOpts) = parent.dependencies.par.map{ dependency =>
      findModule(dependency.coordinates, Some(dependency.hash)) match {
        case Right(Some(module)) => (Some(EvictedModule(module, reason = "parent: " + parent.coordinates + " is intransitive")), None)
        case Right(None) => (None, Some(MissingDependency(dependency, parent.coordinates, reason = "missing (non transitive) module: " + dependency.coordinates)))
        case Left(conflictModules) => throw new Exception("found more than 1 module for: " + dependency + ": " + conflictModules.mkString(",")) //TODO: handle gracefully? (remember to remove all references to it in the code)
      }
    }.unzip
    val evictedModules: mutable.Set[EvictedModule] = mutable.Set.empty ++ evictedModuleOpts.flatten.seq
    val missingDeps: immutable.Set[MissingDependency] = missingDepOpts.flatten.seq

    (mutable.Set.empty[ConfigurationMatcher.MatchedModule], evictedModules, missingDeps) 
  }
  
  def build(confExpr: String, module: Module, configurationMapping: String => String, findModule: Adept.FindModule): Option[MutableTree] = {
    //TODO: @tailrec? 
    def build(parent: Module, isTransitive: Boolean, parentExclusionRules: Set[DependencyExclusionRule], matchedConfigurations: collection.mutable.Set[Configuration]): MutableNode = {
      val extendedConfigurations = matchedConfigurations ++ matchedConfigurations.flatMap(c => ConfigurationResolver.extendedConfs(module.configurations, c) )
      
      val dependencies = if (isTransitive) parent.dependencies else collection.immutable.Set.empty[Dependency]
      val (moduleMatches, evictedModules, missingDependencies) = if (isTransitive) { 
        ConfigurationMatcher.matchingModules(parent.coordinates, dependencies, parentExclusionRules, extendedConfigurations.toSet, configurationMapping, findModule)
      } else {
        findNonTransitive(parent, findModule) 
      }
      val (foundArtifacts, evictedArts) = ConfigurationMatcher.matchingArtifacts(parent.artifacts, extendedConfigurations.toSet)
      
      val dependentNodes = moduleMatches.par.map{ case (module, isTransitive, exclusionRules, confs) => //TODO: verify that par helps
        build(module, isTransitive, exclusionRules, confs)
      }
      MutableNode(module = parent, configurations = matchedConfigurations, artifacts = foundArtifacts, children = dependentNodes.seq, evictedArtifacts = evictedArts, evictedModules = evictedModules, missingDependencies = missingDependencies)
    }
    
    ConfigurationResolver.resolve(module.configurations, confExpr) match {
      case Right(confs) =>
        Some(MutableTree(confExpr, build(module, true, Set.empty, confs))) 
      case Left(msg) =>
        logger.debug("no valid configurations found while building tree. " + msg)
        None
    }
  }
    
}