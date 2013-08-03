package adept.sbt.tasks

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept
import akka.actor.ActorSystem
import adept.sbt.ProgressActors

private[adept] trait AdeptClasspath {
  import akka.util.duration._

  def adeptClasspathTask(sbtConfig: sbt.Configuration) = (adeptTree in sbtConfig, adeptDirectory, adeptTimeout, streams) map { (eitherTree, adeptDirectory, timeoutMinutes, s) =>
    withAdeptClassloader {
      val cachedFiles = eitherTree match {
        case Right(tree) =>
          val cachedArtifacts = tree.artifacts.toSeq.map { a =>
            (a.hash, a.locations) -> (None: Option[java.io.File])
          }
          val timeout = timeoutMinutes.minutes
          Adept.artifact(adeptDirectory, cachedArtifacts, timeout, Some(ProgressActors.get)) match {
            case Right(files) => files
            case Left(error) =>
              throw Incomplete(None, message = Some(error))
          }
        case Left((missingDependencies, _)) =>
          throw Incomplete(None, message = Some("missing dependencies: " + missingDependencies.map(_.descriptor.asCoordinates).mkString("\n")))
      }
      cachedFiles.classpath
    }
  }

}