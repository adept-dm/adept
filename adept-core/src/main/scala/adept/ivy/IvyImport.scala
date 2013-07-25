package adept.ivy

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
import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
import org.apache.ivy.core.module.id.ModuleRevisionId
import adept.ivy.conversions.Modules

object IvyImport extends Logging { //TODO: move Ivy out to separate project?

  import adept.ivy.utils.IvyHelpers._

  /** returns the modules that where added */
  def add(coords: Coordinates, ivy: Ivy, adept: Adept): Set[Module] = {
    logger.trace("building dependency tree from ivy...")
    //TODO: mutable collection is perhaps not ideal, then again ivy is not thread safe either way...
    val allCoords = new collection.mutable.HashSet[Coordinates] with collection.mutable.SynchronizedSet[Coordinates]
    val modules = new collection.mutable.HashSet[Module] with collection.mutable.SynchronizedSet[Module]

    val initIvyReport = ivy.resolve(ModuleRevisionId.newInstance(coords.org, coords.name, coords.version), resolveOptions(), changing)

    initIvyReport.getArtifacts().asScala.foreach {
      case ivyArtifact: IvyArtifact =>
        val ivyId = ivyArtifact.getModuleRevisionId()
        val coords = Coordinates(ivyId.getOrganisation(), ivyId.getName(), ivyId.getRevision())
        Modules.convert(coords, ivy, adept.findModule _, adept.add _)(allCoords, modules) match {
          case Left(error) => 
          logger.error("could not create module for: " + coords)
          case _ => //
        }
    }

    Modules.convert(coords, ivy, adept.findModule _, adept.add _)(allCoords, modules) match {
      case Left(error) =>
        logger.error("could not create module for: " + coords)
      case _ => //
    }

    modules.toSet
  }
}