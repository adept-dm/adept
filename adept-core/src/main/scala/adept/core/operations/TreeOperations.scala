package adept.core.operations

import adept.core.models._
import adept.utils.Logging
import collection.{ Set => _, _ }
import adept.core.Adept
import adept.utils.EitherUtils

private[core] object TreeOperations extends Logging {

  def evict(tree: MutableTree, moduleReasons: Set[(Module, String)]) = {
    def evict(node: MutableNode): Unit = { //TODO: @tailrec?
      node.evict(moduleReasons)
      node.children.foreach(evict)
    }
    tree.children.foreach(evict)
  }

  private def findNonTransitive(parent: Module, configurations: Set[Configuration], configurationMapping: String => String, findModule: Adept.FindModule) = {
    val (evictedModuleOpts, missingDepOpts) = parent.dependencies.par.map { dependency =>
      findModule(dependency.coordinates, dependency.uniqueId, Set.empty) match {
        case Right(Some(module)) => (Some(EvictedModule(module, reason = "parent: " + parent.coordinates + " is intransitive")), None)
        case Right(None) =>
          val mappedConf = configurationMapping(dependency.configuration)
          ConfigurationResolver.resolve(configurations, mappedConf) match {
            case Right(confs) =>
              (None, Some(MissingDependency(dependency, Some(parent.coordinates), required = false, reason = "could not find dependency: " + dependency.coordinates + " declared in: " + parent)))
            case Left(msg) =>
              (None, Some(MissingDependency(dependency, Some(parent.coordinates), required = true, reason = "could not find a dependency evicted because: " + msg)))
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
  def build(parent: Module, isTransitive: Boolean, parentExclusionRules: Set[DependencyExclusionRule],
    matchedConfigurations: collection.mutable.Set[Configuration], configurationMapping: String => String,
    findModule: Adept.FindModule): MutableNode = {

    val extendedConfigurations = (matchedConfigurations ++ matchedConfigurations.flatMap(c => ConfigurationResolver.extendedConfs(parent.configurations, c))).toSet
    val (moduleMatches, evictedModules, missingDependencies) = if (isTransitive) {
      ConfigurationMatcher.matchingModules(parent.coordinates, parent.dependencies, parentExclusionRules, extendedConfigurations, configurationMapping, findModule)
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

  def build(confExpr: String, dependencies: Set[Dependency], universes: Set[Universe], moduleConfigurations: Set[Configuration], configurationMapping: String => String, findModule: Adept.FindModule): Either[Set[(Dependency, String)], MutableTree] = {
    ConfigurationResolver.resolve(moduleConfigurations, confExpr) match {
      case Right(matchingConfs) =>
        var errors = collection.mutable.Set.empty[(Dependency, String)]
        var missing = collection.mutable.Set.empty[MissingDependency]
        var evictedModules = collection.mutable.Set.empty[EvictedModule]
        var children = collection.mutable.Set.empty[MutableNode]
        dependencies.foreach{ dependency => 
          findModule(dependency.coordinates, dependency.uniqueId, universes) match {
            case Right(Some(module)) =>
              val mappedConf = configurationMapping(dependency.configuration)
              ConfigurationResolver.resolve(matchingConfs.toSet, mappedConf, module.configurations) match {
                case Right(confs) =>
                  children += build(module, dependency.isTransitive, dependency.exclusionRules, confs, configurationMapping, findModule)  
                case Left(reason) => evictedModules += EvictedModule(module, "evicted: no matching configurations: " + reason.mkString(","))
              }
            case Right(None) => 
              val reason = "could not find module for: " + dependency + (if (universes.nonEmpty) " in universes: " + universes.mkString(",") else "")
              missing += MissingDependency(dependency, None, true, reason) 
            case Left(foundErrors) => 
              errors += dependency -> ("found errors in the following modules: " + foundErrors.mkString("\n"))
          } 
        }
        if (errors.isEmpty) {
          val tree = MutableTree(confExpr, children.toSet, evictedModules.toSet, missing.toSet)
          Right(tree)
        } else {
          Left(errors.toSet)
        }
      case Left(msg) =>
        logger.debug("no valid configurations found while building tree. " + msg)
        Left(Set.empty)
    }
  }

}