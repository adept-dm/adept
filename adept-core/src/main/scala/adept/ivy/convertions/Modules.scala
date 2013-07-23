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

private[ivy] object Modules extends Logging {
  import adept.ivy.utils.IvyHelpers._

  def convert(initCoords: Coordinates, ivy: Ivy, findModule: Adept.FindModule, add: Module => Unit)(allCoords: collection.mutable.Set[Coordinates], modules: collection.mutable.Set[Module]): Either[String, Module] = {
    val coords = resolveDynamicVersion(initCoords, ivy)
    val report = ivy.resolve(ModuleRevisionId.newInstance(coords.org, coords.name, coords.version), resolveOptions(), changing)
    val moduleDescriptor = report.getModuleDescriptor()

    allCoords += coords
    val parentNode = parent(report)
    val isLoaded = parentNode.getDescriptor != null

    if (isLoaded) {
      val configurations = moduleDescriptor.getConfigurations()
        .map(Configurations.convert(parentNode, _)).toSet

      val attributes: Map[String, Seq[String]] = Map.empty ++
        Option(moduleDescriptor.getDescription()).filter(_.nonEmpty).map(a => "description" -> Seq(a)).toMap ++
        Option(moduleDescriptor.getHomePage()).map(a => "home-page" -> Seq(a)).toMap

      val artifacts = Artifacts.convert(parentNode.getAllArtifacts, parentNode, ivy)

      val uniqueId = UniqueId.default(coords, artifacts)

      val foundModule: Option[Module] =
        findModule(coords, Some(uniqueId)) match {
          case Right(res) => {
            res
          }
          case _ => None
        }

      val adeptModule = foundModule.getOrElse {
        val depNodes = configurations.flatMap { conf =>
          //TODO: will this works for all confs? I am not sure how to get every possible conf 
          parentNode.getDependencies(conf.name, conf.name, "*").asScala.map { case i: IvyNode => i -> conf }
        }
        val deps = Dependencies.convert(depNodes, parentNode, ivy, findModule, add)(allCoords, modules)
        val overrides = Overrides.convert(parentNode, ivy, findModule, add)(allCoords, modules)

        val adeptModule = Module(coords, uniqueId, artifacts, configurations, attributes, deps, overrides)
        modules += adeptModule
        synchronized {
          findModule(adeptModule.coordinates, None) match {
            case Right(None) => add(adeptModule)
            case _ => //do nothing
          }
        }
        adeptModule
      }
      Right(adeptModule)
    } else {
      logger.info("could not load: " + coords + ". problem messages: " + report.getAllProblemMessages().asScala.mkString("\n"))
      Left("could not load: " + coords + ". problem messages: " + report.getAllProblemMessages().asScala.mkString("\n"))
    }

  }

}