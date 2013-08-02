package adept.sbt.tasks

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept

private[adept] trait AdeptRepository {

  val adeptLocalRepositoryTask = (adeptLocalRepositoryName, streams) map { (name, s) =>
    val dir = Path.userHome / ".adept" //TODO: make a setting out of this

    val res = synchronized {
      if (Adept.exists(dir, name))
        Adept.open(dir, name)
      else Adept.init(dir, name)
    }
    res match {
      case Left(msg) =>
        s.log.error(msg)
        None
      case Right(adept) => Some(adept)
    }
  }

}