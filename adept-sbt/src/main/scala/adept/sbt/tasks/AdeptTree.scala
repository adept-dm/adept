package adept.sbt.tasks

import sbt.{ Configuration => _, Node => _, Artifact => _, _ }
import sbt.Keys._
import adept.sbt.AdeptKeys._
import adept.sbt.Utils._
import adept.core.models._
import adept.core.Adept

private[adept] trait AdeptTree {
  import adept.sbt.AdeptKeys._

  def adeptTreeTask(sbtConfig: sbt.Configuration) = (name, adeptModule, adeptUpdate, streams) map { (name, module, all, s) =>
    withAdeptClassloader {

      val configurationMapping: String => String = Configuration.defaultConfigurationMapping(_, "*->default(compile)") //TODO
      val confExpr = sbtConfig.name

      val checkpoint = System.currentTimeMillis()
      Adept.build(all.toSet, confExpr, module, configurationMapping) match {
        case Some(tree) =>
          val resolveTimeSpent = System.currentTimeMillis - checkpoint
          val requiredMissing = tree.requiredMissing
          if (requiredMissing.nonEmpty) {
            Left(requiredMissing.filter(!_.evicted) -> tree)
          } else {
            s.log.success("Resolved dependency tree in (" + name + "): " + resolveTimeSpent + " ms")
            Right(tree)
          }
        case None =>
          throw new Incomplete(None, message = Some("No tree was resolved for: " + name))
      }
    }: Either[(Set[MissingDependency], Tree), Tree]
  }

}