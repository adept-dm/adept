import sbt._
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

  private def adeptCoordinates(dep: ModuleID) = {
    //TODO: fix  this properly
    val name = dep.crossVersion match {
      case _: CrossVersion.Binary => CrossVersion.crossName(dep.name, CrossVersion.TransitionScalaVersion) //TODO: must be another method on crossversion that does this?
      case _: CrossVersion.Full => throw new Exception("NOT IMPLEMENTED: CrossVersion.Full (sbt plugin)") //TODO: fix...
      case _: CrossVersion.Disabled.type => dep.name
    }
    Coordinates(dep.organization, name, dep.revision)
  }
  
  val adeptClasspathTask = (adeptRepositories, adeptDirectory, adeptDependencies, adeptLocalRepository, adeptArtifactTypes, streams) map { (adeptRepositories, adeptDirectory, deps, localRepo, artifactTypes, s) => 
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

      val timeout = 60.minutes

      if (all.isEmpty && deps.nonEmpty) {
        val msg = "no repositories defined for: " + deps.mkString(",")
        s.log.error(msg)
        throw new Exception(msg)
      } else {
        val depModules = all.par.flatMap{ adept =>
          deps.map{ dep =>
            val coords = adeptCoordinates(dep)
            (dep, adept, adept.findModule(coords, hash = None).toSeq) //TODO: hash 
          }
        }

        val modulesOverAllRepos = depModules.seq.groupBy{ case (dep, adept, modules) => dep }.map{ case (dep, allDepsModules) => dep -> allDepsModules.map(_._3).flatten  }
        
        val modulesWithExlusions = modulesOverAllRepos.map{case (dep, modules) =>
          dep -> modules.map{m => 
            if (dep.isTransitive) {
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
        modulesOverAllRepos.foreach{ case (dep, modules) =>
          
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

        val allWithDependencies = depModules.flatMap { case (dep, adept, maybeModule) => 
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
        }
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
    adeptClasspath <<= adeptClasspathTask,
    (managedClasspath in Compile) <++= adeptClasspath
  )

}
