import sbt.{ Configuration => _, _}
import sbt.Keys._
import adept.models._
import adept.Adept
import adept.ivy.IvyHelpers

object AdeptPlugin extends Plugin {
  
  import AdeptKeys._
  
  def withAdeptClassloader[A](f: => A): A = {
    val classloader = Adept.getClass.getClassLoader
    val thread = Thread.currentThread
    val oldLoader = thread.getContextClassLoader

    try {
      thread.setContextClassLoader(classloader)
      f
    } finally {
      thread.setContextClassLoader(oldLoader)
    }
  }

  val adeptIvyAddTask = (adeptLocalRepository, ivyConfiguration, libraryDependencies, streams) map { (localAdept, ivyConfiguration, ivyDeps, s) =>
    val ivySbt = new IvySbt(ivyConfiguration)
    val modules = ivySbt.withIvy(s.log){ ivy =>
      localAdept.toSeq.flatMap { adept =>
        ivyDeps.flatMap{ dep =>
          val coords = adeptCoordinates(dep)
          IvyHelpers.add(coords, ivy, adept)
        }
      }
    }
    modules
  }

  private val DefaultConf = "compile" //TODO: use IvyConfiguration from SBT and use a settingkey
  private val DefaultMappingConf = "*->default(compile)" //TODO: use from SBT and use a settingkey
 
  private def adeptCoordinates(dep: ModuleID): Coordinates = {
    //TODO: fix  this properly
    val name = dep.crossVersion match {
      case _: CrossVersion.Binary => CrossVersion.crossName(dep.name, CrossVersion.TransitionScalaVersion) //TODO: must be another method on crossversion that does this?
      case _: CrossVersion.Full => throw new Exception("NOT IMPLEMENTED: CrossVersion.Full (sbt plugin)") //TODO: fix...
      case _: CrossVersion.Disabled.type => dep.name
    }
    Coordinates(dep.organization, name, dep.revision)
  }
    
  private def adeptDependency(adept: Adept, dep: ModuleID): Option[Dependency]= {
    val coords = adeptCoordinates(dep)
    val module = adept.findModule(coords, hash = None) //TODO: Hash
    module.map{ m =>
      Dependency(coords, m.hash, dep.configurations.getOrElse(DefaultMappingConf))
    }
  }
  
  private def adeptConfiguration(sbtConf: sbt.Configuration): Configuration = {
    val visibility = if (sbtConf.isPublic) Visibility.Public else Visibility.Private
    Configuration(sbtConf.name, Some(sbtConf.description), sbtConf.extendsConfigs.map(_.name).toSet, visibility, None)
  }
  
  
  def adeptClasspathTask(sbtConfig: sbt.Configuration) = (adeptRepositories, adeptDirectory, adeptDependencies, adeptLocalRepository, adeptArtifactTypes, streams) map { (adeptRepositories, adeptDirectory, sbtDeps, localRepo, artifactTypes, s) => 
    
    def isExcluded(module: Module, exclusionRules: Seq[sbt.ExclusionRule]): Boolean ={
      val matchingRules = exclusionRules.find{ exclusionRule =>
        if (exclusionRule.configurations.nonEmpty) throw new Exception("exclusion rule configurations are not implmeneted. got: " + exclusionRule) 
        (exclusionRule.organization, exclusionRule.name) match {
          case ("*", module.coordinates.name) => true 
          case (module.coordinates.org,"") => true
          case (module.coordinates.org,"*") => true
          case (module.coordinates.org,module.coordinates.name) => true
          case (_,_) => false
        }
      }
      matchingRules.foreach{ exclusionRule =>
        s.log.debug("excluding " + module + " because of " + exclusionRule)
      }
      matchingRules.isDefined
    }
    
    withAdeptClassloader{
      import akka.util.duration._

      def repos = Adept.repositories(adeptDirectory)
      
      val uncloned = adeptRepositories.filter{ case (name, uri) => //TODO: should check URI as well?
        !Adept.repositories(adeptDirectory).find(_.name == name).isDefined
      }

      uncloned.foreach{ case (name, url) => //cloning what is not there
        Adept.clone(adeptDirectory, name, url)
      }

      val all = repos.filter{ adept =>
        adeptRepositories.keySet.contains(adept.name)
      }  ++ localRepo.toList


      all.foreach { adept =>
        if (!adept.isLocal) adept.pull()
      }

      val timeout = 60.minutes //TODO: make a setting out of this
      val adeptConfigurations = sbt.Configurations.default.map(adeptConfiguration)
      val confsExpr = sbtConfig.name //TODO: do we need something better here?
      val matchingConfs = Adept.resolveConfigurations(confsExpr, adeptConfigurations.toSet)

      if (all.isEmpty && sbtDeps.nonEmpty) {
        val msg = "no repositories defined for: " + sbtDeps.mkString(",")
        s.log.error(msg)
        throw new Exception(msg)
      } else {
        
        val resolvedDependencies = all.flatMap{ adept => //TODO: would .par be quicker?
          sbtDeps.map{sbtDep => 
            val maybeDep = adeptDependency(adept, sbtDep)
            //println(sbtDep + "   confsExpr " + confsExpr +  "    ----  "  + matchingConfs.map(_.name))
            val maybeResolvedDeps = maybeDep.map{ dep =>
              if (sbtDep.isTransitive) {
                val resolvedModules = adept.resolveModules(Set(dep), matchingConfs, confsExpr).filter{ case (module, confs) =>
                  !isExcluded(module, sbtDep.exclusions)
                }
                resolvedModules 
              } else adept.findModule(adeptCoordinates(sbtDep)).map(_ -> matchingConfs).toSeq
            }
           
            (sbtDep, maybeResolvedDeps)
          }
        }            
        val resolvedModuleConfs = resolvedDependencies.seq.groupBy{ case (sbtDep, maybeResolvedDeps) =>
          sbtDep
        }.flatMap{ case (sbtDep, found) =>
          val resolvedModuleConfs = found.flatMap(_._2).flatten
          //sbtDep.configurations
          //this sbtDep matches the current conf expr
          val conf = sbtDep.configurations.getOrElse{ //either test or *->default(compile)
            "compile" //FIXME: this will break!
          }
          //println(conf + " " + sbtDep)
          val matchesConfs = confsExpr == conf
          //if (matchesConfs && resolvedModuleConfs.isEmpty) throw new Exception("could not find any modules for dependency: " + sbtDep)
          resolvedModuleConfs
        }
        
        val resolvedModules = resolvedModuleConfs.map(_._1).toSeq
        val prunedModules = Adept.resolveConflicts(resolvedModules)
        
        val artifactInfo = prunedModules.flatMap{ module =>
          val confs = resolvedModuleConfs(module)
          val confsExpr = confs.map(_.name).mkString(";")
          val artifactModules = Adept.resolveArtifacts(module.artifacts, module.configurations, confsExpr).map(_ -> module)
          artifactModules
        }
        val mergedLocations = artifactInfo.map{ case (artifact, module) =>
          ((artifact.hash, module.coordinates, artifact.locations), None)
        }
        //println(mergedLocations)
        Adept.artifact(adeptDirectory, mergedLocations.toSeq, timeout) match {
          case Left(error) => throw new Exception(error)
          case Right(files) => files.classpath
        }
        
        //Configuration.modules()
        
        /*
        val modulesOverAllRepos = depAdeptDep.seq.groupBy{ case (dep, adept, maybeAdeptDep) => dep }.map{ case (dep, allDepsAdeptDeps) => dep -> allDepsAdeptDeps.map(_._3).flatten  }
        
        val modulesWithExlusions = modulesOverAllRepos.map{case (sbtDep, adeptDeps) =>
          sbtDep -> adeptDeps.map{m => 
            if (sbtDep.isTransitive) {
              val excludedDeps = m.dependencies.filter{ d =>
                val foundExclusion = dep.exclusions.find{ e =>
                  //TODO: configurations/artifacts?
                  (e.name.isEmpty && d.coords.org == e.organization) || (e.organization == d.coords.org && e.name == d.coords.name)
                }
                foundExclusion.foreach{ e => s.log.debug("excluding: " + e  +" as " +d + " in "+ m) }
                foundExclusion.isDefined
              }
              m.copy(dependencies = excludedDeps)
            } else {
              m.copy(dependencies = Set.empty)
            }
          }
        }
        
        //verify declared dependencies:
        modulesWithExlusions.foreach{ case (dep, modules) =>
          
          //check coords:
          val allCoords = modules.map(_.coordinates).distinct
          if (allCoords.size > 1) {
            val msg = "found more than one set of coordinates for same dependency: "+dep+". These were found: " + allCoords.mkString(",")
            s.log.error(msg)
            throw new Exception("cannot load adept dependencies because: "+msg)
          } else if (allCoords.size == 0) {
            val msg = "could not find the dependency: "+dep + " in repositories " + all.mkString(",")
            s.log.error(msg)
            throw new Exception("cannot load adept dependencies because: "+msg)
          }
          //check hashes:
          val allHashes = modules.map(_.hash).distinct
          if (allHashes.size > 1) {
            val msg = "found more than 1 hash for same dependency: "+dep+". These were found: " + allHashes.mkString(",")
            s.log.error(msg)
            throw new Exception("cannot load adept dependencies because: "+msg)
          }
        }
        
        val allWithDependencies = depAdeptDep.flatMap { case (dep, adept, maybeModule) =>
            maybeModule.flatMap{ module =>
              adept.dependencies(module) + module
            }
        }

        val prunedModules = Adept.resolveConflicts(allWithDependencies.seq)

        val mergedLocations = prunedModules.groupBy(_.coordinates).flatMap{ case (coords, modules) =>
          val currentArtifacts = modules.flatMap(_.artifacts.filter(a => artifactTypes.contains(a.artifactType)))
          currentArtifacts.map{ currentArtifact =>
            (currentArtifact.hash, coords , currentArtifact.locations) -> None
          }
        }

        val res = Adept.artifact(adeptDirectory, mergedLocations.toSeq, timeout)
        res match {
          case Left(msg) => 
            s.log.error(msg)
            throw new Exception("cannot get dependencies because: "+msg)
          case Right(jars) => jars.classpath
        }*/
      } 
    }
  }

  def adeptSettings = Seq(
    adeptArtifactTypes := defaultArtifactTypes,
    adeptDirectory := Path.userHome / ".adept",
    adeptRepositories := Map(
      
    ),
    adeptDependencies := Seq(
    ),
    adeptLocalRepository <<= streams map { s =>
      val dir = Path.userHome / ".adept"
      val name = "local"
      val res = if (Adept.exists(dir, name))
        Adept.open(dir, name)
      else Adept.init(dir, name)
      if (res.isLeft) {
        val msg = res.left.get
        s.log.error(msg)
        throw new Exception(msg)
      }
      else Some(res.right.get)
    },
    adeptIvyAdd <<= adeptIvyAddTask,
    adeptClasspath in Compile <<= adeptClasspathTask(Compile),
    (managedClasspath in Compile) <++= adeptClasspath in Compile,
    adeptClasspath in Test <<= adeptClasspathTask(Test),
    (managedClasspath in Test) <++= adeptClasspath in Test
  )

}
