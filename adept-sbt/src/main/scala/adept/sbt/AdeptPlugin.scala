
import sbt._
import sbt.Keys._
import adept.models._
import adept.Adept

object AdeptPlugin extends Plugin {

  val adeptDependencies = SettingKey[Seq[ModuleID]]("adept-dependencies", "declares dependencies fetched by adept")
  val adeptRepositories = SettingKey[Map[String,String]]("adept-repositories", "adept the name and git url for the adept repositories")
  val adeptLocalRepository = TaskKey[Option[Adept]]("adept-local-repository", "the local repository for adept")
  val adeptClasspath = TaskKey[Classpath]("adept-classpath")
  val adeptDirectory = SettingKey[File]("adept-directory")


  //TODO: adept-describe-classpath
  
  
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

  val adeptClasspathTask = (adeptRepositories, adeptDirectory, adeptDependencies, adeptLocalRepository, streams) map { (adeptRepositories, adeptDirectory, deps, localRepo, s) => 
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
            val coords = Coordinates(dep.organization, dep.name, dep.revision)
            (dep, adept, adept.findModule(coords, hash = None).toSeq) //TODO: hash 
          }
        }
        val modulesOverAllRepos = depModules.seq.groupBy{ case (dep, adept, modules) => dep }.map{ case (dep, allDepsModules) => dep -> allDepsModules.map(_._3).flatten  }

        //verify declared dependencies:
        modulesOverAllRepos.foreach{ case (dep, modules) =>
          //check coords:
          val allCoords = modules.map(_.coords).distinct
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
          val allHashes = modules.map(_.artifact.hash).distinct
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

        val prunedModules = adept.operations.Prune(allWithDependencies.seq)

        val mergedLocations = prunedModules.groupBy(_.coords).map{ case (coords, modules) =>
          val hashes = modules.map(_.artifact.hash).distinct
          if (hashes.size != 1){
            val msg = "found more than 1 hash for what should be the modules: "+modules+". These were found: " + hashes.mkString(",")
            s.log.error(msg)
            throw new Exception("cannot load dependencies because: "+msg)
          }
          (hashes.head, coords , modules.flatMap(_.artifact.locations).toSet) -> None
        }

        val res = Adept.artifact(adeptDirectory, mergedLocations.toSeq, timeout)
        if (res.isLeft) {
          val msg = res.left.get
          s.log.error(msg)
          throw new Exception("cannot get dependencies because: "+msg)
        } else res.right.get.classpath
      }
    }
  }
  
  override val settings = Seq(
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

    adeptClasspath <<= adeptClasspathTask,
    (managedClasspath in Compile) <++= adeptClasspath
  )

}
