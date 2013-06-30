import sbt.{ Configuration => _, _}
import sbt.Keys._
import adept.core.models._
import adept.core.Adept
import adept.ivy.IvyHelpers

/**
  * Workflow:
  * 0) update metadata
  * 1) read dependencies from all repositories
  * 2) merge found dependencies
  * 2) transitively resolve (transitive) dependencies based on configuration. skip non-transitive dependencies
  * 4) check that all dependencies have been found
  * 3) exclude exclusions
  * 5) resolve version conflicts
  * 6) check for unresolved conflicts
  * 7) add explicit artifacts
  * 8) check that all explicit artifacts could be added
  */
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

 
  private def adeptCoordinates(dep: ModuleID): Coordinates = {
    //TODO: fix  this properly
    val name = dep.crossVersion match {
      case _: CrossVersion.Binary => CrossVersion.crossName(dep.name, CrossVersion.TransitionScalaVersion) //TODO: must be another method on crossversion that does this?
      case _: CrossVersion.Full => throw new Exception("NOT IMPLEMENTED: CrossVersion.Full (sbt plugin)") //TODO: fix...
      case _: CrossVersion.Disabled.type => dep.name
    }
    Coordinates(dep.organization, name, dep.revision)
  }
    
  private def adeptDependency(adept: Adept, dep: ModuleID, configurationMapping: String): Option[Dependency]= {
    val coords = adeptCoordinates(dep)
    val module = adept.findModule(coords, hash = None) //TODO: Hash
    module.map{ m =>
      Dependency(coords, m.hash, dep.configurations.getOrElse(configurationMapping))
    }
  }
  
  private def adeptConfiguration(sbtConf: sbt.Configuration): Configuration = {
    val visibility = if (sbtConf.isPublic) Visibility.Public else Visibility.Private
    Configuration(sbtConf.name, Some(sbtConf.description), sbtConf.extendsConfigs.map(_.name).toSet, visibility, None)
  }

  
  def adeptClasspathTask(sbtConfig: sbt.Configuration) = (adeptRepositories, adeptDirectory, adeptDependencies, adeptLocalRepository, adeptArtifactTypes, adeptTimeout, defaultConfigurationMapping in GlobalScope, streams) map { (adeptRepositories, adeptDirectory, allSbtDeps, localRepo, artifactTypes, timeoutMinutes, defaultConfiguration, s) => 
    def isExcluded(module: Module, exclusionRules: Seq[sbt.ExclusionRule]): Boolean ={ //TODO: add exclusions into Adept core
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
      


      val timeout = timeoutMinutes.minutes //TODO: make a setting out of this
      val adeptConfigurations = sbt.Configurations.default.map(adeptConfiguration).toSet
      val confMapping: String => String = Configuration.defaultConfigurationMapping(_, "*->default(compile)")
      val conf = confMapping(sbtConfig.name) //TODO: do we need something better 
      //*->default(compile) mapping comes from: ivy/CustomXmlParser.scala
      //33:         if(defaultConfig.isDefined) setDefaultConfMapping("*->default(compile)")

      val matchingConfs = Adept.resolveConfigurations(conf, adeptConfigurations)

      val sbtDeps = allSbtDeps.map{ sbtDep =>
        val conf = sbtDep.configurations.getOrElse(defaultConfiguration)
        sbtDep.copy(configurations = Some(confMapping(conf))) //TODO: am I sure we want to do this mapping here or inside of adept? we ned for resovleDependencyConfiguration later on
      }
      
      if (all.isEmpty && sbtDeps.nonEmpty) {
        val msg = "no repositories defined for: " + sbtDeps.mkString(",")
        s.log.error(msg)
        throw new Exception(msg)
      } else {
        
        val resolvedDependencies = all.flatMap{ adept => //TODO: would .par be quicker?
          sbtDeps.map{sbtDep => 
            val maybeDep = adeptDependency(adept, sbtDep, sbtDep.configurations.getOrElse(defaultConfiguration))
            val maybeResolvedDeps = maybeDep.map{ dep =>
              if (sbtDep.isTransitive) {
                val resolvedModules = adept.resolveModules(Set(dep), adeptConfigurations, conf, confMapping).filter{ case (module, confs) =>
                  !isExcluded(module, sbtDep.exclusions)
                }
                println(resolvedModules.map(_._1.coordinates) + ":::::: " + resolvedModules.map(_._2.map(_.name)))
                resolvedModules
              } else {
                val matchingConfs = adeptConfigurations.filter(c => c.name == sbtConfig.name).toSet
                if (matchingConfs.nonEmpty) {
                  adept.findModule(adeptCoordinates(sbtDep)).map(_ -> matchingConfs).toSeq
                }else {
                  Seq()
                }
              }
            }
           
            (sbtDep, maybeResolvedDeps)
          }
        }            
        val resolvedModuleConfs = resolvedDependencies.seq.groupBy{ case (sbtDep, maybeResolvedDeps) =>
          sbtDep
        }.flatMap{ case (sbtDep, found) =>
          val resolvedModuleConfs = found.flatMap(_._2).flatten
          val matchedConfs = resolvedModuleConfs.flatMap { case (module, confs) =>
            val matched = Adept.resolveDependencyConfigurations(sbtDep.configurations.get, adeptConfigurations, confs, module)
            matched
          }
          if (matchedConfs.nonEmpty && resolvedModuleConfs.isEmpty) throw new Exception("could not find any modules for dependency: " + sbtDep)
          resolvedModuleConfs
        }
        
        val resolvedModules = resolvedModuleConfs.map(_._1)
        val prunedModules = Adept.resolveConflicts(resolvedModules.toSeq)
        
        val artifactInfo = prunedModules.flatMap{ module =>
          val confs = resolvedModuleConfs(module)
          val confsExpr = confMapping(confs.map(_.name).mkString(";")) //TODO: we must change the way this works: we are doing confMapping all over the place. inside of adept, outside when checking for confs, ...
          val artifactModules = Adept.resolveArtifacts(module.artifacts, confs, confsExpr).map(_ -> module)
          println(module.coordinates + "   " + confsExpr + " << " + confs.map(_.name) + " >> " + module.artifacts + "  << "  +  artifactModules.map(_._1.locations))
          artifactModules
        }
        val mergedLocations = artifactInfo.map{ case (artifact, module) =>
          ((artifact.hash, module.coordinates, artifact.locations), None)
        }
        Adept.artifact(adeptDirectory, mergedLocations.toSeq, timeout) match {
          case Left(error) => throw new Exception(error)
          case Right(files) => files.classpath
        }
        
      } 
    }
  }

  def adeptSettings = Seq(
    adeptConfigurationMapping := "*->default(compile)", 
    adeptArtifactTypes := defaultArtifactTypes,
    adeptDirectory := Path.userHome / ".adept",
    adeptTimeout := 60, //minutes
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
