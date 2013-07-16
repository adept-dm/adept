package adept.core.operations

import adept.core.models._
import adept.utils.Logging
import collection.{ Set => _, _ }
import adept.core.Adept

private[core] object TreeOperations extends Logging {

  def evict(tree: MutableTree, moduleReasons: Set[(Module, String)]) = {
    def evict(node: MutableNode): Unit = { //TODO: @tailrec?
      node.evict(moduleReasons)
      node.children.foreach(evict)
    }
    evict(tree.root)
  }
  
  private def findNonTransitive(parent: Module, configurations: Set[Configuration], configurationMapping: String => String, findModule: Adept.FindModule) = {
    val (evictedModuleOpts, missingDepOpts) = parent.dependencies.par.map { dependency =>
      findModule(dependency.coordinates, dependency.uniqueId) match {
        case Right(Some(module)) => (Some(EvictedModule(module, reason = "parent: " + parent.coordinates + " is intransitive")), None)
        case Right(None) =>
          val mappedConf = configurationMapping(dependency.configuration)
          ConfigurationResolver.resolve(configurations, mappedConf) match {
            case Right(confs) =>
              (None, Some(MissingDependency(dependency, parent.coordinates, evicted = false, reason = "could not find dependency: " + dependency.coordinates + " declared in: " + parent)))
            case Left(msg) =>
              (None, Some(MissingDependency(dependency, parent.coordinates, evicted = true, reason = "could not find a dependency evicted because: " + msg)))
          }
        case Left(conflictModules) => throw new Exception("found more than 1 module for: " + dependency + ": " + conflictModules.mkString(",")) //TODO: handle gracefully? (remember to remove all references to it in the code)
      }
    }.unzip
    val evictedModules: mutable.Set[EvictedModule] = mutable.Set.empty ++ evictedModuleOpts.flatten.seq
    val missingDeps: mutable.Set[MissingDependency] = mutable.Set.empty ++ missingDepOpts.flatten.seq

    (mutable.Set.empty[ConfigurationMatcher.MatchedModule], evictedModules, missingDeps)
  }

  //TODO: @tailrec?
  //TODO: there is code smell in the number of params. consider refactoring
  def build(parent: Module, isTransitive: Boolean, parentExclusionRules: Set[DependencyExclusionRule], matchedConfigurations: collection.mutable.Set[Configuration], configurationMapping: String => String, findModule: Adept.FindModule): MutableNode = {
    val extendedConfigurations = (matchedConfigurations ++ matchedConfigurations.flatMap(c => ConfigurationResolver.extendedConfs(parent.configurations, c))).toSet
    val dependencies = if (isTransitive) parent.dependencies else collection.immutable.Set.empty[Dependency]
    val (moduleMatches, evictedModules, missingDependencies) = if (isTransitive) {
      ConfigurationMatcher.matchingModules(parent.coordinates, dependencies, parentExclusionRules, extendedConfigurations, configurationMapping, findModule)
    } else {
      findNonTransitive(parent, extendedConfigurations, configurationMapping, findModule)
    }
    val (foundArtifacts, evictedArts) = ConfigurationMatcher.matchingArtifacts(parent.artifacts, extendedConfigurations.toSet)

    val dependentNodes = moduleMatches.par.map {
      case (module, isTransitive, exclusionRules, confs) => //TODO: verify that par helps
        build(module, isTransitive, exclusionRules, confs, configurationMapping, findModule)
    }
    MutableNode(module = parent, configurations = matchedConfigurations, artifacts = foundArtifacts, children = dependentNodes.seq, evictedArtifacts = evictedArts, evictedModules = evictedModules, overriddenDependencies = mutable.Set.empty, missingDependencies = missingDependencies, postBuildInsertReason = None)
  }

  def build(confExpr: String, module: Module, configurationMapping: String => String, findModule: Adept.FindModule): Option[MutableTree] = {
    ConfigurationResolver.resolve(module.configurations, confExpr) match {
      case Right(confs) =>
        Some(MutableTree(confExpr, build(module, true, Set.empty, confs, configurationMapping, findModule)))
      case Left(msg) =>
        logger.debug("no valid configurations found while building tree. " + msg)
        None
    }
  }

}