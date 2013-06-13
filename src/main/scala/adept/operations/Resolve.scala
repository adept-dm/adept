package adept.operations

import adept.models._
import adept.utils.Logging
import adept.Adept

case class NotFoundDependenciesException(dependencies: Set[Dependency]) extends Exception("could not find dependencies: " + dependencies.map(_.coords))

private[adept] object Resolve extends Logging {

  def extendedConfs(rootConfs: Set[Configuration], conf: Configuration): Set[Configuration]= {
    val (doesExtend, doesNotExtend) = rootConfs.partition{ rootConf =>
      conf.extendsFrom.contains(rootConf.name)
    }
    doesExtend ++ doesExtend.flatMap{ extendedConf =>
      extendedConfs(doesNotExtend, extendedConf)
    }
  }
  
  def resolveConfigurations(confs: Set[String], configurations: Set[Configuration]): Set[Configuration]= {
    configurations.filter(c => confs.contains(c.name))
  }
  
  def findArtifactConf(artifact: Artifact, configurations: Set[Configuration], conf: Configuration) = {
    conf.deprecated.foreach{ msg =>
      logger.warn(conf+" for artifact "+artifact +" is deprecated: " + msg)
    }
    val resolved = resolveConfigurations(artifact.configurations, configurations)
    val allArtifactConfsExtended = resolved ++ resolved.flatMap(extendedConfs(configurations, _))
    allArtifactConfsExtended.find(c => c == conf) -> allArtifactConfsExtended
  }
  
  /**
   *  The artifacts matching a configuration expression for a given module
   * 
   *  Expression must be either a configuration name in the module or '*' which represents all artifacts (for all configurations)
   */
  def artifacts(artifacts: Set[Artifact], configurations: Set[Configuration], confExpr: String): (Seq[Artifact], Seq[EvictedArtifact]) = {
    val allArtifactsWithEvicted: Seq[(Option[EvictedArtifact], Artifact)] = 
      artifacts.flatMap{ artifact =>
        splitConfs(confExpr).map{ 
          case "*" => None -> artifact
          case "" => None -> artifact //tread empty as *
          case confName => configurations.find( _.name == confName).map{ conf =>
            if (conf.visibility == Visibility.Private) {
              val msg = "artifact " + artifact + " has visibility of conf '" + conf.name + "' is private"
              logger.debug("evicting artifact: " + msg)
              Some(EvictedArtifact(artifact, msg))
            } else {
              findArtifactConf(artifact, configurations, conf) match {
                case (None, allConfs) => {
                  val msg = artifact + " does not match any of the configurations: " + allConfs.map(_.name).mkString(",")
                  logger.debug("evicting artifact: " + msg)
                  Some(EvictedArtifact(artifact, reason = msg))
                }
                case _ => {
                  logger.debug("including artifact: " + artifact)
                  None
                }
              }
            }
          }.getOrElse{
            val msg = "could not find a configuration named '" + confName + "' in " + configurations.map(_.name).mkString(",")
            logger.debug("evicting artifact: " + msg)
            Some(EvictedArtifact(artifact, msg))
          } -> artifact
        }
      }.toSeq
      
    val (included, evicted) = allArtifactsWithEvicted.partition {
      case (None, _) => true //None means no eviction
      case _ => false
    }
    val cleanIncluded = included.map{ case (_, a) => a}
    val cleanEvicted = evicted.flatMap{ case (e, _) => e}
    //println(cleanEvicted.map(_.reason))
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
  
  private def splitDependencyConfExpr(confExpr: String) = {
    val parts = confExpr.split("->")
    if (parts.size < 2) throw new Exception("could not parse configuration '" + confExpr + "' because it was expected to contain a least one '->'. (the confs was mapped using a mapper)") 
    if (parts.size > 2) throw new Exception("could not parse configuration '" + confExpr + "' because it has more than 2 parts seperated by '->'. (the confs was mapped using a mapper)") 
    (parts.head, parts.last)
  }
  
  val ConfSep = ";"
    
  def splitConfs(confsExpr: String) = {
    confsExpr.split(ConfSep).toSeq
  }
  
  private def findFallback(confExpr: String): (String, Option[String]) = {
    val FallbackExpr = """(.*?)\((.*?)\)$""".r
    confExpr.trim match {
      case FallbackExpr(rest, fallbackExpr) => rest -> Some(fallbackExpr)
      case _ => confExpr -> None
    }
  }
  private val Comma = ","
  
  def findMatchingConfs(confExpr: String, confs: Set[Configuration]): Set[Configuration]= {
    val foundConfs = confs.filter{ conf =>
      confExpr.split(Comma).map(_.trim).find{
        case "*" => true 
        case conf.name => true
        case _ => false
      }.nonEmpty
    }
    val foundExtendedConfs = foundConfs.flatMap(extendedConfs(confs, _))
    val all = foundConfs ++ foundExtendedConfs
    //println("all matching : " + all.map(_.name))
    all
    
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
   * 
   * @parm dependencies the input dependencies
   * @param configurations the configurations to use
   * @return the modules found from the dependencies and the confExpr found from the dependency. if the dependency has test->compile(*) and the current conf is test, the returned expression will be compile(*). it also returns the evicted modules
   */
  //@throws(NotFoundDependenciesException.getClass) triggers eclipse error!
  def allModules(dependencies: Set[Dependency], configurations: Set[Configuration], findModule: (Coordinates, Option[Hash]) => Option[Module]): Seq[(Module, Set[Configuration])] ={ //TODO: evicted modules
    val modules = {
      val maybeModules = dependencies.par.map{ dependency => //TODO: check if this makes things faster. we do this because of findModule which reads from disk/use a IO execution context
        val maybeModule = findModule(dependency.coords, None) //FIXME: there is a serious issue here because we are not storing the dependency hashes correctly
        maybeModule -> dependency
      }
      val notFound = maybeModules.collect{
        case (None, d) => {
          d
        }
      }
      
      if (notFound.nonEmpty) {
        throw new NotFoundDependenciesException(notFound.seq) //TODO: should be recursive and find all. Could also return an either instead? This way the caller can actually do something about the missing dependencies.
      }
      
      maybeModules.collect{
        case (Some(module), dependency) => {
          logger.debug("found module for dependency: " + dependency)
          val depConfExprs = splitConfs(dependency.configuration) //e.g. split "runtime->*;test->default,compile" or something simple as "*->*"
          //println(module.coordinates + "--->" + dependency.configuration)
          //check from-configuration, map each of them, search to-configuration for valid configurations
          val confs = depConfExprs.flatMap{ basicDepConfExpr => 
            val depConfExpr = Dependency.defaultConfMapping(basicDepConfExpr)
            val (from, toParts) = splitDependencyConfExpr(depConfExpr)
            val moduleConfs = onlyVisible(module.configurations) //drop dependent modules that are not in visible from root
            //println(basicDepConfExpr + "~'" + from + "'VS'"+ toParts + "'--->"+configurations.map(_.name))
            findMatchingConfs(from, configurations).flatMap{ matchingFromConf =>
              //println("matched conf: '" + matchingFromConf.name +"' " + dependency.coords)
              logger.debug("matched conf: '" + matchingFromConf.name +"' " + dependency.coords)
              toParts.split(Comma).flatMap{ to =>
                val (restTo, fallbackConfExpr) = findFallback(to)
                val matchingConfs = findMatchingConfs(restTo, moduleConfs)
                if (matchingConfs.isEmpty) {
                  logger.debug("no matching confs for '" + restTo + "' " + dependency.coords)
                  val foundFallbackConfs = fallbackConfExpr.toSet.flatMap{ c: String =>
                    logger.debug("finding matching confs for fallback conf: '" + c + "' in " +  moduleConfs.map(_.name))
                    findMatchingConfs(c, moduleConfs)
                  }
                  if (foundFallbackConfs.isEmpty) {
                    logger.debug("no fallback confs in " + fallbackConfExpr +" found for " + dependency.coords)
                  }
                  foundFallbackConfs
                } else {
                  logger.debug("'" + restTo + "' matched  " + module.configurations.map(_.name) + " for " + dependency.coords)
                  matchingConfs
                }
              }
              
            }
          }
          module -> confs.toSet
        }
      }
    }
    val onlyMatching =  modules.filter(_._2.nonEmpty)
    val dependentModules = onlyMatching.flatMap{ case (module, confs) =>
      allModules(module.dependencies, confs, findModule)
    }
    onlyMatching.seq.toSeq ++ dependentModules.seq.toSeq
  }
  
  
}