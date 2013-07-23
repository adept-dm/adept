package adept.ivy.convertions

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
import org.apache.ivy.core.module.id.ModuleRevisionId

private[ivy] object Dependencies extends Logging {
  import adept.ivy.utils.IvyHelpers._

  def convert(depNodes: Set[(IvyNode, Configuration)], parentNode: IvyNode, ivy: Ivy, findModule: Adept.FindModule, add: Module => Unit)(allCoords: collection.mutable.Set[Coordinates], modules: collection.mutable.Set[Module]): Set[Dependency] = {
    val allCoordinates = depNodes.map {
      case (node, conf) =>
        val coords = resolveDynamicVersion(Coordinates(node.getId.getOrganisation(), node.getId.getName, node.getId.getRevision()), ivy)

        val depReport = ivy.resolve(ModuleRevisionId.newInstance(coords.org, coords.name, coords.version), resolveOptions(), changing) //make sure this artifact has been resolved
        (node, coords, conf)
    }
    val allDependencies = for {
      (node, coords, conf) <- allCoordinates
    } yield {
      logger.info(allCoords.size + " found so far. " + coords + " has not yet been found")
      val depDescriptor = node.getDependencyDescriptor(parentNode)

      if (depDescriptor.getAllIncludeRules() != null && depDescriptor.getAllIncludeRules().nonEmpty) throw new Exception("Include rules are not implemented") //TODO: <- fix this

      val exclusionRules = depDescriptor.getAllExcludeRules().map { ivyExcludeRule =>
        val org = ivyExcludeRule.getId().getModuleId().getOrganisation()
        val name = ivyExcludeRule.getId().getModuleId().getName()
        DependencyExclusionRule(org, name)
      }.toSet

      val modConfs = depDescriptor.getModuleConfigurations
      val configuration = modConfs.map { modConf =>
        val depConfs = depDescriptor.getDependencyConfigurations(modConf)
        confString(modConf, depConfs)
      }.mkString(";")

      allCoords += coords
      logger.info("adding " + coords + " to allCoords")
      Dependency(coords, None, configuration, force = depDescriptor.isForce(), isTransitive = depDescriptor.isTransitive(), exclusionRules = exclusionRules)
    }
    allDependencies
  }
}