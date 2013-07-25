package adept.sbt.tasks

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept

private[adept] trait AdeptClasspath {
  import akka.util.duration._
  
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

}