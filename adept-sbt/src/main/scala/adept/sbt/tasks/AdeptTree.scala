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
      Adept.resolve(all.toSet, confExpr, module.dependencies, module.universes, module.configurations, configurationMapping) match {
        case Right(tree) =>
          val resolveTimeSpent = System.currentTimeMillis - checkpoint
          val requiredMissing = tree.requiredMissing
          if (requiredMissing.nonEmpty) {
            s.log.debug("dependencies not found:\n" + requiredMissing.filter(!_.required).map(_.descriptor.asCoordinates).mkString("\n"))
            Left(requiredMissing.filter(!_.required) -> tree)
          } else {
            s.log.success("Resolved dependency tree in (" + name + "): " + resolveTimeSpent + " ms")
            Right(tree)
          }
        case Left(errors) =>
          val errorString = errors.map{ case (dep, msg) =>
            dep + ": " + msg
          }.mkString("\n")
          throw new Incomplete(None, message = Some("Found errors while resolving tree: " + errorString))
      }
    }: Either[(Set[MissingDependency], Tree), Tree]
  }

}