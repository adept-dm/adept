package adept.sbt.tasks

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept
import akka.util.FiniteDuration
import akka.util.duration._
import adept.utils.Logging

private[adept] trait AdeptAdd extends Conversions {

  val adeptInputAddTask = inputTask { (argTask: TaskKey[Seq[String]]) =>
    (argTask, adeptArtifactLocations, organization, name, version, sbtVersion, sbtPlugin, scalaVersion,
      publishArtifact, adeptModule, adeptDirectory, packagedArtifacts,
      publishTo, streams, adeptTimeout) map {
        (args: Seq[String], artifactLocations, organization, name, version, sbtVersion, sbtPlugin, scalaVersion,
        publishArtifact, module, adeptDirectory, packagedArtifacts, publishTo, s, timeoutInMinutes) =>
          val repoNames = args
          if (repoNames.isEmpty) {
            throw new Incomplete(None, message = Some("No repositories defined for: " + name))
          } else if (publishArtifact) {
            adeptAddTask(artifactLocations = artifactLocations, organization = organization, name = name, version = version,
              sbtVersion = sbtVersion, sbtPlugin = sbtPlugin, scalaVersion = scalaVersion,
              publishTo = publishTo, noArtifactsModule = module, repoNames = repoNames,
              adeptDirectory = adeptDirectory, packagedArtifacts = packagedArtifacts,
              logger = s.log)(timeoutInMinutes.minutes)
          } else {
            s.log.debug("skipping artifacts since publishArtifact is false")
            Seq.empty
          }
      }
  }

  def adeptAddTask(artifactLocations: Map[String, String],
    organization: String, name: String, version: String,
    sbtVersion: String, sbtPlugin: Boolean, scalaVersion: String,
    publishTo: Option[sbt.Resolver], noArtifactsModule: Module,
    repoNames: Seq[String], adeptDirectory: File,
    packagedArtifacts: Map[sbt.Artifact, sbt.`package`.File],
    logger: sbt.Logger)(timeout: FiniteDuration): Seq[(Module, File)] = {

    withAdeptClassloader {
      val artifactPatterns = publishTo.collect {
        case repo: PatternsBasedRepository =>
          repo.patterns.artifactPatterns
        case _ =>
          Seq.empty
      }
      val artifacts = packagedArtifacts.map { //accessing the artifacts here, because AdeptModule should not have to package artifacts to proceed 
        case (artifact, file) => adeptArtifact(
          artifactLocations = artifactLocations,
          artifact = artifact,
          file = file,
          name = name,
          version = version,
          organization = organization,
          sbtVersion = sbtVersion,
          scalaVersion = scalaVersion,
          artifactPatterns = artifactPatterns,
          logger = logger,
          sbtPlugin = sbtPlugin)(timeout)
      }.toSet

      val module = noArtifactsModule.copy(artifacts = artifacts)

      val repos = repoNames.flatMap { name =>
        Adept.open(adeptDirectory, name) match {
          case Right(adept) => Some(adept)
          case Left(error) =>
            logger.error(error)
            None
        }
      }

      repos.flatMap { adept =>
        adept.add(module) match {
          case Right(file) => Some(module -> file)
          case Left(file) =>
            logger.error("could not create module for " + module.coordinates + " in " + file)
            None
        }
      }

    }
  }
}