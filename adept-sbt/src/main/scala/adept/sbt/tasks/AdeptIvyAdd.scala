package adept.sbt.tasks

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept
import adept.ivy.IvyImport

private[adept] trait AdeptIvyAdd extends Conversions {
  val adeptIvyAddTask = (adeptLocalRepository, ivyConfiguration, adeptDependencies, scalaVersion, streams) map { (localAdept, ivyConfiguration, deps, scalaVersion, s) =>
    val ivySbt = new IvySbt(ivyConfiguration)
    val modules = ivySbt.withIvy(s.log) { ivy =>
      import collection.JavaConverters._
      localAdept.toSeq.flatMap { adept =>
        deps.flatMap { dep =>
          val coords = adeptCoordinates(dep, scalaVersion)
          IvyImport.add(coords, ivy, adept)
        }
      }
    }
    modules
  }
}