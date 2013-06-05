package adept.models

import adept.utils.Logging

object Visibility extends Enumeration {
  val Public = Value
  val Private = Value
}

case class Configuration(name: String, description: Option[String], extendsFrom: Set[String], visibility: Visibility.Value, deprecated: Option[String])

class Evicted(val reason: String)
case class EvictedArtifact(artifact: Artifact, module: Module, override val reason: String) extends Evicted(reason)
case class EvictedDependency(dependency: Dependency, module: Module, override val reason: String) extends Evicted(reason)

object Configuration extends Logging {
  private[models] def extendedConfs(rootConfs: Set[Configuration], conf: Configuration): Set[Configuration]= {
    val (doesExtend, doesNotExtend) = rootConfs.partition{ rootConf =>
      conf.extendsFrom.contains(rootConf.name)
    }
    doesExtend ++ doesExtend.flatMap{ extendedConf =>
      extendedConfs(doesNotExtend, extendedConf)
    }
  }
  
  private def findConf(artifact: Artifact, module: Module, conf: Configuration) = {
    conf.deprecated.foreach{ msg =>
      logger.warn(conf+" for artifact "+artifact +" is deprecated: " + msg)
    }
    val allConfs = Set(conf.name) ++ extendedConfs(module.configurations, conf).map(_.name)
    artifact.configurations.find(c => allConfs.contains(c)) -> allConfs 
  }
  
  /**
   *  The artifacts matching a configuration expression for a given module
   * 
   *  Expression must be either a configuration name in the module or '*' which represents all artifacts (for all configurations)
   */
  def artifacts(module: Module, confExpr: String): (Seq[Artifact], Seq[EvictedArtifact]) = {

    val allArtifactsWithEvicted: Seq[(Option[EvictedArtifact], Artifact)] = 
      module.artifacts.flatMap{ artifact =>
        splitConfs(confExpr).map{ 
          case "*" => None -> artifact
          case confName => module.configurations.find( _.name == confName).map{ conf =>
            if (conf.visibility == Visibility.Private) {
              val msg = "artifact " + artifact + " has visibility of conf '" + conf.name + "' is private"
              logger.debug("evicting artifact: " + msg)
              Some(EvictedArtifact(artifact, module, msg))
            } else {
              findConf(artifact, module, conf) match {
                case (None, allConfs) => {
                  val msg = artifact + " does not match any of the configurations: " + allConfs.mkString(",")
                  logger.debug("evicting artifact: " + msg)
                  Some(EvictedArtifact(artifact, module, reason = msg))
                }
                case _ => {
                  logger.debug("including artifact: " + artifact)
                  None
                }
              }
            }
          }.getOrElse{
            val msg = "could not find a configuration named '" + confName + "' in " + module.configurations.map(_.name).mkString(",")
            logger.debug("evicting artifact: " + msg)
            Some(EvictedArtifact(artifact, module, msg))
          } -> artifact
        }
      }.toSeq
      
    val (included, evicted) = allArtifactsWithEvicted.partition {
      case (None, _) => true //None means no eviction
      case _ => false
    }
    val cleanIncluded = included.map{ case (_, a) => a}
    val cleanEvicted = evicted.flatMap{ case (e, _) => e}
    cleanIncluded -> cleanEvicted
  }
  
  private def moduleFor(dependency: Dependency, modules: Seq[Module]): Module = {
    val foundModules = modules.filter(m => m.hash == dependency.hash && m.coordinates == dependency.coords)
    if (foundModules.isEmpty) {
      throw new Exception("no modules found for " + dependency + " among: " + modules.map(m => m.coordinates + "-hash(" + m.hash + ")").mkString(","))
    } else if (foundModules.size > 1) {
      throw new Exception("found too many modules found for " + dependency + " ("+ foundModules.map(m => m.coordinates + "-hash(" + m.hash + ")").mkString(",") +") among: " + modules.mkString(","))
    } else {
      foundModules.head
    }
  }
  
  private def splitConfExpr(dependency: Dependency, confExpr: String) = {
    val parts = confExpr.split("->")
    if (parts.size < 2) throw new Exception("could not parse configuration '" + confExpr + "' in " + dependency + " because it was expected to contain a least one '->'. (the confs was mapped using a mapper)") 
    if (parts.size > 2) throw new Exception("could not parse configuration '" + confExpr + "' in " + dependency + " because it has more than 2 parts seperated by '->'. (the confs was mapped using a mapper)") 
    (parts.head, parts.last)
  }
  
  val ConfSep = ";"
    
  private def splitConfs(confExpr: String) = {
    confExpr.split(ConfSep).toSeq
  }
  
  private def matchConfExpr(confExpr1: String, confExpr2: String): Boolean = {
    (confExpr1, confExpr2) match {
      case ("*", _) => true
      case (_, "*") => true
      case (`confExpr2`, _) => true
      case _ => false
    }
  }
  
  private def findFallback(confExpr: String): (String, Option[String]) = {
    val FallbackExpr = """(.*?)\((.*?)\)$""".r
    confExpr.trim match {
      case FallbackExpr(rest, fallbackExpr) => rest -> Some(fallbackExpr)
      case _ => confExpr -> None
    }
  }
  
  private def findMatchingConfs(confExpr: String, confs: Set[Configuration]): Set[Configuration]= {
    confs.filter{ conf =>
      confExpr match {
        case "*" => true 
        case conf.name => true
        case _ => false
      }
    }
  }
  
  def onlyVisible(confs: Set[Configuration]) = {
    val privateConfs = confs.filter(_.visibility == Visibility.Private)
    if (privateConfs.nonEmpty) logger.debug("ignoring following confs which are private: " + privateConfs.map(_.name).mkString(","))
    confs.filter(_.visibility != Visibility.Private)
  }
  
  def expression(confs: Set[Configuration]) = confs.map(_.name).mkString(ConfSep)
  
  /**
   * The modules and matching configurations for a set of dependencies and a configuration expression
   * All dependencies must have a corresponding module in allResolvedModules
   * 
   * A configuration expression can be a configuration name or the wildcard char: '*'   
   * 
   * The supported dependency expressions are currently:
   * - '*' means all configurations for the current module
   * - ';' separates multiple configurations
   * - 'A->B' means that configuration(s) matched by A in the parent module is mapped to B in the module described by the dependency    
   * - using a fallback configuration, e.g.'A->B(C)'. It means that if B is not found in the dependent module, the configuration found by C will be used. C can be either a name or a wildcard, '*'
   * 
   * The following Ivy syntaxes are _currently_ not supported:
   * - '#'
   * - '%'
   * - '@'
   * - '!' (e.g. '*, !A, !B->X')
   * - '[attr=expr]'
   */
  def modules(dependencies: Set[Dependency], allResolvedModules: Seq[Module], confsExpr: String): (Seq[(Module, Set[Configuration])], Seq[EvictedDependency]) = {
    logger.debug("finding confs: " + confsExpr + " in " + dependencies.mkString(","))
    val res = dependencies.toSeq.flatMap{ dependency: Dependency =>
      logger.debug("checking " + dependency)
      val dependentModule  = moduleFor(dependency, allResolvedModules)
      val mappedConfs = Dependency.defaultConfMapping(dependency.configuration)
      splitConfs(confsExpr).flatMap{ confExpr =>
        splitConfs(mappedConfs).map{ mappedConf =>
          val (fromConf, toConfExpr) = splitConfExpr(dependency, mappedConf)
          if (!matchConfExpr(confExpr, fromConf)) {
            val msg = "could not find a conf for " + dependency + " that matches '" + confExpr + "' with '" +fromConf + "'"
            logger.debug("evicting module: " + msg)
            Some(EvictedDependency(dependency, dependentModule, 
                reason = msg)
                ) -> None
          } else {
            val (restToConfExpr, maybeFallbackConfExpr) = findFallback(toConfExpr)
            val maybeConfs = findMatchingConfs(restToConfExpr, dependentModule.configurations)
            logger.debug("matches found: " + maybeConfs.map(_.name).mkString(",") + " for " + restToConfExpr + " in " + dependentModule)
            if (maybeConfs.isEmpty) {
              val fallbackConfs = maybeFallbackConfExpr.toSet.flatMap{ fallbackConfExpr: String => 
                findMatchingConfs(fallbackConfExpr, dependentModule.configurations)
              }
              if (fallbackConfs.isEmpty) {
                val msg = "did not find any matching confs nor fallback confs for " + toConfExpr + " in " + dependentModule
                logger.debug("evicting module: " + msg)
                Some(EvictedDependency(dependency, dependentModule, 
                    reason = msg)
                    ) -> None
              } else {
                logger.debug("including module: found fallback confs: " + fallbackConfs.map(_.name).mkString(",") + " for " + maybeFallbackConfExpr)
                None -> Some(dependentModule -> onlyVisible(fallbackConfs))
              }
            } else {
                logger.debug("including module: found direct confs: " + maybeConfs.map(_.name).mkString(","))
                None -> Some(dependentModule -> onlyVisible(maybeConfs))
            }
          }
        }
      }
    }
    
    val (evicted, moduleConfs) = res.partition{
      case (None, _) => false
      case (_, None) => true
      case somethingElse => throw new Exception("got an illegal pair: " + somethingElse +  " while partitioning modules and evictedmodules. " + res.mkString(","))
    }
    val cleanModuleConfs = moduleConfs.flatMap{
      case (_, mc) => mc
    }
    val cleanEvicted = evicted.flatMap{
      case (e, _) => e
    }
    cleanModuleConfs -> cleanEvicted
  }
  
  //TODO: @tailrec or non-recursive to avoid stackoverflows?
  def allArtifacts(module: Module, allResolvedModules: Seq[Module], confExpr: String): (Seq[Artifact], Seq[Evicted]) ={
    val (artifacts, evictedArtifacts) = Configuration.artifacts(module, confExpr)
    val (dependentModules, evictedModules) = Configuration.modules(module.dependencies, allResolvedModules, confExpr)
    val (dependentArtifacts, evictedDependentArtifacts) = dependentModules.map{ case (module, confs) =>
      val confExpr = Configuration.expression(confs)
      allArtifacts(module, allResolvedModules, confExpr)
    }.unzip
    
    (artifacts ++ dependentArtifacts.flatten, evictedArtifacts ++ evictedArtifacts ++ evictedDependentArtifacts.flatten)
  }
}
