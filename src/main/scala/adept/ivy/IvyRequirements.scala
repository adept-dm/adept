package adept.ivy

import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import org.apache.ivy.core.module.descriptor.DependencyDescriptor
import adept.resolution.models.Requirement
import org.apache.ivy.core.module.id.ModuleRevisionId
import adept.resolution.models.Variant
import org.apache.ivy.core.module.descriptor.ExcludeRule

object IvyRequirements {
  import IvyConstants._
  import IvyUtils._
  import adept.ext.AttributeDefaults._

  val unsupportedStrings = Set("%", "!", "[", "]", "@", "#")

  private def findFallback(confExpr: String): (String, Option[String]) = {
    val FallbackExpr = """(.*?)\((.*?)\)$""".r
    confExpr.trim match {
      case FallbackExpr(rest, fallbackExpr) => rest.trim() -> Some(fallbackExpr.trim())
      case _ => confExpr.trim() -> None
    }
  }

  private def matchConf(name: String, other: String) = {
    other match {
      case "*" => true
      case `name` => true
      case _ => false
    }
  }

  private def matchVariant(mrid: ModuleRevisionId, variant: Variant): Boolean = {
    val moduleId = mrid.getModuleId()
    variant.attribute(IvyNameAttribute).values == Set(moduleId.getName()) &&
      variant.attribute(IvyOrgAttribute).values == Set(moduleId.getOrganisation()) &&
      variant.attribute(VersionAttribute).values == Set(mrid.getRevision())
  }


  private def getAllConfigurations(module: ModuleDescriptor, confName: String): Set[String] = {
    def getAllConfigurations(module: ModuleDescriptor, existing: Set[String]): Set[String] = {
      existing.flatMap { confName =>
        val newConfs = (module.getConfiguration(confName).getExtends().toSet) + confName
        getAllConfigurations(module, newConfs.diff(existing)) ++ newConfs
      }
    }
    getAllConfigurations(module, Set(confName))
  }

  private def convertDescriptor2Requirements(descriptor: DependencyDescriptor, allConfExprs: Set[String], allIvyImportResults: Set[IvyImportResult]) = {
    var requirements = Set.empty[Requirement]
    allIvyImportResults.foreach { result =>
      if (matchVariant(descriptor.getDependencyRevisionId, result.variant)) {
        val resultConfs = result.variant.attribute(ConfigurationAttribute).values
        allConfExprs.foreach { confExpr =>
          resultConfs.foreach { resultConf =>
            if (matchConf(resultConf, confExpr)) {
              val exclusions = for {
                otherResult <- allIvyImportResults
                excludeRule <- descriptor.getAllExcludeRules()
                if matchesExcludeRule(excludeRule, otherResult.variant)
              } yield {
                otherResult.variant.id
              }
              val newRequirement = Requirement(result.variant.id, Set.empty, exclusions)
              requirements += newRequirement
            }
          }
        }
      }
    }
    requirements
  }

  def convertIvyAsRequirements(module: ModuleDescriptor, allIvyImportResults: Set[IvyImportResult]): Map[String, Set[Requirement]] = {
    var requirements = Map.empty[String, Set[Requirement]]

    //pass 1: convert everything to requirements
    module.getDependencies().foreach { descriptor =>
      descriptor.getModuleConfigurations().foreach { confName =>
        descriptor.getDependencyConfigurations(confName).foreach { configurationExpr =>
          val allValidConfigExprs = {
            configurationExpr.split(",").flatMap { possibleFallbackExpr =>
              if (unsupportedStrings.exists(illegal => possibleFallbackExpr.contains(illegal))) throw new Exception("Cannot process configuration: " + configurationExpr + " in " + descriptor + " because it contains part of a string we do not support: " + unsupportedStrings)
              val (rest, fallback) = findFallback(possibleFallbackExpr)
              getAllConfigurations(module, rest) ++ fallback.toSet[String].flatMap(getAllConfigurations(module, _))
            }
          }
          val newRequirements = convertDescriptor2Requirements(descriptor, allValidConfigExprs.toSet, allIvyImportResults)
          val formerRequirements = requirements.getOrElse(confName, Set.empty[Requirement])
          requirements += confName -> (formerRequirements ++ newRequirements)
        }
      }
    }

    //pass 2: expand the requirements of each configuration (instead of only having test -> */config/test, we have test -> (*/config/test AND */config/compile etc etc))
    module.getDependencies().foreach { descriptor =>
      descriptor.getModuleConfigurations().foreach { confName =>
        val allRequirements = getAllConfigurations(module, confName).flatMap { expandedConfName =>
          requirements.getOrElse(expandedConfName, Set.empty[Requirement])
        }
        requirements += confName -> allRequirements

      }
    }
    requirements
  }
}