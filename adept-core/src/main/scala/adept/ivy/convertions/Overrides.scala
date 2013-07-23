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
import org.apache.ivy.core.module.descriptor.{ Artifact => IvyArtifact }
import org.apache.ivy.core.module.descriptor.{ Configuration => IvyConfiguration }
import org.apache.ivy.plugins.matcher.MapMatcher
import org.apache.ivy.core.module.descriptor.OverrideDependencyDescriptorMediator
import org.apache.ivy.plugins.matcher.PatternMatcher

private[ivy] object Overrides extends Logging {
  import adept.ivy.utils.IvyHelpers._

  def convert(parentNode: IvyNode, ivy: Ivy, findModule: Adept.FindModule, add: Module => Unit)(allCoords: collection.mutable.Set[Coordinates], modules: collection.mutable.Set[Module]): Set[Override] = {
    parentNode.getDescriptor().getAllDependencyDescriptorMediators().getAllRules().asScala.map {
      case (matcher: MapMatcher, overrideMediator: OverrideDependencyDescriptorMediator) =>
        val matcherName = matcher.getPatternMatcher().getName()
        matcherName match {
          case PatternMatcher.EXACT => true
          case _ => throw new Exception("found matcher: " + matcherName + ". currently only: " + PatternMatcher.EXACT + " is supported. parent:" + parentNode)
        }
        val org = matcher.getAttributes.get(IvyPatternHelper.ORGANISATION_KEY) match { case s: String => s }
        val name = matcher.getAttributes.get(IvyPatternHelper.MODULE_KEY) match { case s: String => s }
        val overriddenVersion = overrideMediator.getVersion()

        val coords = Coordinates(org, name, overriddenVersion)
        if (!allCoords.contains(coords)) {
          allCoords += coords
          logger.info("adding override " + coords + " to allCoords")
          Override(coords.org, coords.name, coords.version, None)
        } else {
          modules.find(_.coordinates == coords) match {
            case Some(module) =>
              val uniqueId = module.uniqueId
              Override(coords.org, coords.name, coords.version, Some(uniqueId))
            case None =>
              Override(coords.org, coords.name, coords.version, None)
          }
        }
    }.toSet
  }
}