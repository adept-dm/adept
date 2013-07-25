package adept.ivy.conversions

import adept.core.models._
import org.apache.ivy._
import org.apache.ivy.core._
import org.apache.ivy.core.resolve._
import org.apache.ivy.core.report.ResolveReport
import scala.util._
import java.io.File
import adept.core.Adept
import org.apache.ivy.util.Message
import org.apache.ivy.util.DefaultMessageLogger
import adept.utils.Logging
import collection.JavaConverters._
import org.apache.ivy.core.report.ResolveReport
import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }

case class ExpectedResolveException(msg: String) extends Exception(msg)

private[ivy] object Artifacts {
  import adept.ivy.utils.IvyHelpers._

  def convert(ivyArtifacts: Array[IvyArtifact], parentNode: IvyNode, ivy: Ivy) = {
    val artifacts = ivyArtifacts.toList.flatMap {
      case a: IvyArtifact =>
        val artifactReports = ivy.resolve(parentNode.getId(), resolveOptions(a.getConfigurations().toList: _*), changing).getAllArtifactsReports()
        val thisModuleArtifactReports = artifactReports.filter(_.getArtifact().getId() == a.getId())
        thisModuleArtifactReports.map { r =>
          val artifact = r.getArtifact()
          if (r.getArtifactOrigin() == null) throw new ExpectedResolveException("could not find the location for the artifact: " + artifact + " from parent: " + parentNode)
          val location = r.getArtifactOrigin().getLocation()
          val file = r.getLocalFile()
          val artifactType = artifact.getType()
          (file, location, artifactType) -> artifact.getConfigurations().toList
        }
    }.toSet
    artifacts.groupBy(_._1).flatMap {
      case ((file, location, artifactType), all) =>
        if (file != null && file.exists) {
          val confs = all.flatMap { case (_, c) => c }
          Set(Artifact.fromFile(file, artifactType, confs, Set(location)))
        } else throw new Exception("could not find: " + file + " for " + parentNode)
    }.toSet
  }
}