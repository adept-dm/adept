import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.core.models._
import adept.core.Adept
import adept.ivy._
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

  val adeptIvyAddTask = (adeptLocalRepository, ivyConfiguration, adeptDependencies, scalaVersion, streams) map { (localAdept, ivyConfiguration, deps, scalaVersion, s) =>
    val ivySbt = new IvySbt(ivyConfiguration)
    val modules = ivySbt.withIvy(s.log) { ivy =>
      localAdept.toSeq.flatMap { adept =>
        deps.flatMap { dep =>
          val coords = adeptCoordinates(dep, scalaVersion)
          IvyImport.add(coords, ivy, adept)
        }
      }
    }
    modules
  }

  private def adeptCoordinates(dep: ModuleID, scalaVersion: String): Coordinates = {
    //TODO: fix  this properly
    val name = dep.crossVersion match {
      case _: CrossVersion.Binary => CrossVersion.crossName(dep.name, scalaVersion) //TODO: must be another method on crossversion that does this?
      case _: CrossVersion.Full => throw new Exception("NOT IMPLEMENTED: CrossVersion.Full (sbt plugin)") //TODO: fix...
      case _: CrossVersion.Disabled.type => dep.name
    }
    Coordinates(dep.organization, name, dep.revision)
  }

  private def adeptExclusion(dep: ModuleID, sbtExclusion: sbt.ExclusionRule): DependencyExclusionRule = {
    if (sbtExclusion.configurations.nonEmpty) throw new Exception("configurations for exclusions are not supported: " + sbtExclusion + " in " + dep)
    else {
      val org = sbtExclusion.organization
      val name = sbtExclusion.name
      DependencyExclusionRule(org, name)
    }
  }
  
  private val scalaExclusionRule = DependencyExclusionRule("org.scala-lang", "scala-library") 

  private def adeptDependency(adept: Adept, dep: ModuleID, configurationMapping: String, scalaVersion: String): Option[Dependency] = {
    val coords = adeptCoordinates(dep, scalaVersion)
    val exclusions = dep.exclusions.map(adeptExclusion(dep, _)).toSet + scalaExclusionRule //remove scala, because it is added by sbt
    adept.findModule(coords, uniqueId = None) match { //TODO: change ModuleID to include unique ids as well
      case Right(moduleOpt) => moduleOpt.map { m => Dependency(coords, Some(m.uniqueId), dep.configurations.getOrElse(configurationMapping), isTransitive = dep.isTransitive, force = true, exclusionRules = exclusions) }
      case Left(errorModules) => throw new Exception("Found too many matching modules: " + coords + " " + errorModules.mkString(","))
    }
  }

  private def adeptConfiguration(sbtConf: sbt.Configuration): Configuration = {
    val visibility = if (sbtConf.isPublic) Visibility.Public else Visibility.Private
    Configuration(sbtConf.name, Some(sbtConf.description), sbtConf.extendsConfigs.map(_.name).toSet, visibility, None)
  }

  def adeptTreeTask(sbtConfig: sbt.Configuration) = (name, organization, version, adeptRepositories, adeptDirectory, adeptDependencies, adeptLocalRepository, adeptConfigurationMapping, defaultConfigurationMapping in GlobalScope, scalaVersion, streams) map { (name, organization, version, adeptRepositories, adeptDirectory, allSbtDeps, localRepo, defaultDependencyConf, defaultConfiguration, scalaVersion, s) =>
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
        val dependencies = all.par.flatMap { adept => //TODO: IO context?
          adeptDependency(adept, sbtDep, defaultDependencyConf, CrossVersion.binaryScalaVersion(scalaVersion))
        }
        if (dependencies.isEmpty) notFound += sbtDep
        dependencies
      }.toSet

      if (notFound.nonEmpty) {
        val msg = "could not find the following dependencies for " + name + ":\n" + notFound.mkString("\n")
        s.log.error(msg)
        None
      } else {
        val coords = Coordinates(organization, name, version) //TODO: ProjectID instead?
        val artifacts = Set.empty[Artifact] //TODO: should we have some artifacts in this module?
        val uniqueId = UniqueId.default(coords, new java.util.Date, artifacts)
        val parent = Module(coordinates = coords, uniqueId = uniqueId, configurations = configurations, dependencies = adeptDependencies,
          artifacts = artifacts,
          overrides = Set.empty,
          attributes = Map.empty) //TODO: should we have some attributes in this module?

        val checkpoint = System.currentTimeMillis()
        val tree = Adept.build(all.toSet, confExpr, parent, configurationMapping)
        val resolveTimeSpent = System.currentTimeMillis - checkpoint
        s.log.success("Resolved dependency tree in (" + name + "): " + resolveTimeSpent + " ms")
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
              s.log.error(error)
              Seq.empty
          }
        case None =>
          s.log.error("could not find any adept dependencies in tree")
          Seq.empty
      }
      cachedFiles.classpath
    }
  }

  val LocalRepoName = "local" //TODO: make setting out of this

  def adeptSettings = Seq(
    adeptConfigurationMapping := "compile->compile(*),master(*);runtime->runtime(*)",
    adeptDirectory := Path.userHome / ".adept",
    adeptTimeout := 60, //minutes
    adeptRepositories := Map(),
    adeptDependencies := Seq(),
    adeptLocalRepository <<= streams map { s =>
      val dir = Path.userHome / ".adept" //TODO: make a setting out of this
      val name = LocalRepoName
      val res = if (Adept.exists(dir, name))
        Adept.open(dir, name)
      else Adept.init(dir, name)
      res match {
        case Left(msg) =>
          s.log.error(msg)
          throw new Exception(msg)
        case Right(adept) => Some(adept)
      }
    },
    adeptIvyAdd <<= adeptIvyAddTask,
    //TODO: find a way to do this for all configurations - this sort of sucks!
    adeptTree in Compile <<= adeptTreeTask(Compile),
    adeptTree in Runtime <<= adeptTreeTask(Runtime),
    adeptTree in Test <<= adeptTreeTask(Test),
    adeptClasspath in Compile <<= adeptClasspathTask(Compile),
    adeptClasspath in Runtime <<= adeptClasspathTask(Runtime),
    adeptClasspath in Test <<= adeptClasspathTask(Test),
    (managedClasspath in Compile) <++= adeptClasspath in Compile,
    (managedClasspath in Runtime) <++= adeptClasspath in Runtime,
    (managedClasspath in Test) <++= adeptClasspath in Test)

}
