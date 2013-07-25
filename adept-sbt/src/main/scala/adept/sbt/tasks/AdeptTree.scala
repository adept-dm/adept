package adept.sbt.tasks

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept

private[adept] trait AdeptTree {
  import adept.sbt.AdeptKeys._

  def adeptTreeTask(sbtConfig: sbt.Configuration) = (name, adeptModule, adeptUpdate, streams) map { (name, maybeModule, all, s) =>
    withAdeptClassloader {
      maybeModule.flatMap { module =>
        val configurationMapping: String => String = Configuration.defaultConfigurationMapping(_, "*->default(compile)") //TODO
        val confExpr = sbtConfig.name

        val checkpoint = System.currentTimeMillis()
        val tree = Adept.build(all.toSet, confExpr, module, configurationMapping)
        
        val resolveTimeSpent = System.currentTimeMillis - checkpoint
        tree foreach { _ =>
            s.log.success("Resolved dependency tree in (" + name + "): " + resolveTimeSpent + " ms")
        }
        tree
      }
    }
  }

}