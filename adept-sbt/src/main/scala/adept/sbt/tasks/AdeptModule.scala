package adept.sbt.tasks

import sbt.{ Configuration => _, Hash => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept
import org.apache.ivy.core.IvyPatternHelper
import org.apache.ivy.plugins.resolver.URLResolver

private[adept] trait AdeptModule extends Conversions {

  private def getStructure(state: State) = Project.extract(state).structure

  private def evaluateTask[A](key: TaskKey[A], ref: ProjectRef, state: State): Either[String, A] = {
    //TODO: sbt evaluate task creates tmp files in a non-thread safe manner because sometimes this fails!
    EvaluateTask(getStructure(state), key, state, ref, EvaluateTask defaultConfig state) match {
      case Some((_, Value(a))) => Right(a)
      case Some((_, Inc(inc))) => Left("Error evaluating task '%s': %s".format(key.key, Incomplete.show(inc.tpe)))
      case None => Left("Undefined task '%s' for '%s'!".format(key.key, ref.project))
    }
  }

  val adeptModuleTask = (name, organization, version, adeptUpdate, adeptDependencies, adeptConfigurationMapping, scalaVersion, sbtPlugin, sbtVersion, thisProjectRef, buildDependencies, thisProject, state, streams) map { (name, organization, version, repos, allSbtDeps, defaultDependencyConf, scalaVersion, sbtPlugin, sbtVersion, ref, buildDependencies, project,  state, s) =>
    withAdeptClassloader {
      import akka.util.duration._
      val configurations = project.configurations.map(adeptConfiguration).toSet

      val notFound = new collection.mutable.HashSet[ModuleID]()
      val adeptDependencies = allSbtDeps.flatMap { sbtDep =>
        val dependencies = repos.par.flatMap { adept => //TODO: IO context?
          adeptDependency(adept, sbtDep, defaultDependencyConf, CrossVersion.binaryScalaVersion(scalaVersion))
        }
        if (dependencies.isEmpty) notFound += sbtDep
        dependencies
      }.toSet

      val buildDepRefs = buildDependencies.classpath(ref)
      
      if (buildDepRefs.nonEmpty) {
        s.log.warn("adding all dependencies from dependent projects on " + name + " (see TODO in code )") 
      }
      
      //TODO: the module dependencies should NOT be part of the module!  move it to adept classpath?
      val moduleDependencies = buildDepRefs flatMap { thatProjectDep =>
        evaluateTask(adeptModule, thatProjectDep.project, state) match {
          case Right(module) =>
            (module.dependencies + Dependency(module.coordinates, Some(module.uniqueId), "provided->compile(*),master(*)")).toSeq //TODO: make a setting of provided setting
          case Left(error) =>
            throw new Incomplete(None, message = Some(error))
        }
      }

      if (notFound.nonEmpty) {
        throw new Incomplete(None, message = Some("could not find the following dependencies for " + name + ":\n" + notFound.mkString("\n")))
      } else {
        val artifacts = Set.empty[Artifact]

        val coords = Coordinates(organization.toLowerCase, name.toLowerCase, version) //TODO: ProjectID instead?
        val uniqueId = UniqueId.default(coords, new java.util.Date, artifacts)
        val universes = {
          val scalaVerse = scalaUniverse(scalaVersion)
          if (sbtPlugin) Set(scalaVerse, sbtUniverse(sbtVersion))
          else Set(scalaVerse)
        }
        val module = Module(coordinates = coords, uniqueId = uniqueId,
          universes = universes, configurations = configurations,
          dependencies = adeptDependencies ++ moduleDependencies,
          artifacts = artifacts,
          overrides = Set.empty,
          attributes = Map.empty) //TODO: should we have some attributes in this module?
        module
      }
    }
  }
}
