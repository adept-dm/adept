package adept.sbt.tasks

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept

private[adept] trait AdeptUpdate {

  def adeptUpdateTask = (adeptDirectory, adeptRepositories, adeptLocalRepository, streams) map { (adeptDirectory, adeptRepositories, localRepo, s) =>
    //defaultConfigurationMapping in GlobalScope, 
    withAdeptClassloader {
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
        if (!adept.isLocal) {
          val success = adept.pull()
          if (!success) {
            s.log.error("cannot pull to: " + adept)
          }
        }
      }
      all: Seq[Adept]
    }
  }
}