import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.core.models._
import adept.core.Adept
import adept.ivy.IvyHelpers
import akka.util.duration._

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
    val modules = ivySbt.withIvy(s.log) { ivy =>
      localAdept.toSeq.flatMap { adept =>
        ivyDeps.flatMap { dep =>
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

  private def adeptDependency(adept: Adept, dep: ModuleID, configurationMapping: String): Option[Dependency] = {
    val coords = adeptCoordinates(dep)
    adept.findModule(coords, uniqueId = None) match { //TODO: change ModuleID to include unique ids as well
      case Right(moduleOpt) => moduleOpt.map { m => Dependency(coords, Some(m.uniqueId), dep.configurations.getOrElse(configurationMapping)) }
      case Left(errorModules) => throw new Exception("Found too many matching modules: " + coords + " " + errorModules.mkString(","))
    }
  }

  private def adeptConfiguration(sbtConf: sbt.Configuration): Configuration = {
    val visibility = if (sbtConf.isPublic) Visibility.Public else Visibility.Private
    Configuration(sbtConf.name, Some(sbtConf.description), sbtConf.extendsConfigs.map(_.name).toSet, visibility, None)
  }

  def adeptTreeTask(sbtConfig: sbt.Configuration) = (name, organization, version, adeptRepositories, adeptDirectory, adeptDependencies, adeptLocalRepository, adeptConfigurationMapping, defaultConfigurationMapping in GlobalScope, streams) map { (name, organization, version, adeptRepositories, adeptDirectory, allSbtDeps, localRepo, defaultDependencyConf, defaultConfiguration, s) =>
    withAdeptClassloader {
      import akka.util.duration._

      def repos = Adept.repositories(adeptDirectory)

      val uncloned = adeptRepositories.filter {
        case (name, uri) => //TODO: should check URI as well?
          !Adept.repositories(adeptDirectory).find(_.name == name).isDefined
      }

      uncloned.foreach {
        case (name, url) => //cloning what is not there
          Adept.clone(adeptDirectory, name, url)
      }

      val all = repos.filter { adept =>
        adeptRepositories.keySet.contains(adept.name)
      } ++ localRepo.toList

      all.foreach { adept =>
        if (!adept.isLocal) adept.pull()
      }
      
      val configurations = sbt.Configurations.default.map(adeptConfiguration).toSet
      val configurationMapping: String => String = Configuration.defaultConfigurationMapping(_, "*->default(compile)") //TODO
      val confExpr = sbtConfig.name

      val notFound = new collection.mutable.HashSet[ModuleID]()
      val adeptDependencies = allSbtDeps.flatMap { sbtDep =>
        val dependencies = all.par.flatMap { adept =>
          adeptDependency(adept, sbtDep, defaultDependencyConf)
        }
        if (dependencies.isEmpty) notFound += sbtDep
        dependencies
      }.toSet

      if (notFound.nonEmpty) {
        s.log.error("could not find the following dependencies:\n" + notFound.mkString("\n"))
        None
      } else {
        val coords = Coordinates(organization, name, version)
        val artifacts = Set.empty[Artifact] //TODO: artifacts?
        val uniqueId = UniqueId.default(coords, new java.util.Date, artifacts)
        val parent = Module(coordinates = coords,  uniqueId = uniqueId, configurations = configurations, dependencies = adeptDependencies,
          artifacts = artifacts, 
          overrides = Set.empty, 
          attributes = Map.empty) //TODO: attributes?

        val checkpoint = System.currentTimeMillis()
        val tree = Adept.build(all.toSet, confExpr, parent, configurationMapping)
        val resolveTimeSpent = System.currentTimeMillis - checkpoint
        s.log.info("resolved adept tree in: " + resolveTimeSpent + " ms")
        tree
      }
    }
  }

  def adeptClasspathTask(sbtConfig: sbt.Configuration) = (adeptTree in sbtConfig, adeptDirectory, adeptTimeout, streams) map { (maybeTree, adeptDirectory, timeoutMinutes, s) => 
    withAdeptClassloader {
      val cachedFiles = maybeTree match {
        case Some(tree) =>
          val cachedArtifacts = tree.artifacts.toSeq.map { a =>
            (a.hash, a.locations) -> (None: Option[java.io.File])
          }
          val timeout = timeoutMinutes.minutes
          Adept.artifact(adeptDirectory, cachedArtifacts, timeout) match {
            case Right(files) => files
            case Left(error) => 
              Seq.empty
          }
        case None =>
          s.log.error("could not find any adept dependencies in tree")
          Seq.empty
      }
      cachedFiles.classpath
    }
  }

  def adeptSettings = Seq(
    adeptConfigurationMapping := "compile->compile(*),master(*);runtime->runtime(*)",
    adeptDirectory := Path.userHome / ".adept",
    adeptTimeout := 60, //minutes
    adeptRepositories := Map(),
    adeptDependencies := Seq(),
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
      } else Some(res.right.get)
    },
    adeptIvyAdd <<= adeptIvyAddTask,
    adeptTree in Compile <<= adeptTreeTask(Compile), 
    adeptTree in Test <<= adeptTreeTask(Test), 
    adeptClasspath in Compile <<= adeptClasspathTask(Compile),
    adeptClasspath in Test <<= adeptClasspathTask(Test),
    (managedClasspath in Compile) <++= adeptClasspath in Compile,
    (managedClasspath in Test) <++= adeptClasspath in Test)

}
