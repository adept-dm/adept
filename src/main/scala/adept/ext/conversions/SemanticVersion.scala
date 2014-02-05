package adept.ext.conversions

import adept.repository.models.ConfiguredVariantsMetadata
import adept.models._
import adept.ext.AttributeDefaults

class SemanticVersion(ids: Set[Id]) extends Conversion {

  private lazy val SemanticVersionRegEx = """^(\d+)\.?(\d+)\.?(\\*|\d+)$""".r //TODO: change so that SNAPSHOTS are prereleasee (and matches reg ex)

  def convert(configuredVariant: ConfiguredVariantsMetadata): Option[ConfiguredVariantsMetadata] = {
    val versions = configuredVariant.attributes.find(_.name == AttributeDefaults.VersionAttribute) match {
      case Some(Attribute(AttributeDefaults.VersionAttribute, values)) => values.headOption.flatMap { version =>
        version match {
          case SemanticVersionRegEx(major, minor, point) => Some((major, minor, point))
          case _ => None
        }
      }
    }
    val attributes = versions match {
      case Some((major, minor, _)) => configuredVariant.attributes + Attribute(AttributeDefaults.BinaryVersionAttribute, Set(major + "." + minor))
      case _ => configuredVariant.attributes
    }

    val configurations = configuredVariant.configurations.map { configuration =>
      val requirements = configuration.requirements.flatMap { requirement =>
        if (ids(requirement.id)) {
          val constraints = requirement.constraints.find(_.name == AttributeDefaults.VersionAttribute) match {
            case Some(Constraint(AttributeDefaults.VersionAttribute, values)) if values.size == 1 => values.headOption.flatMap { version =>
              version match {
                case SemanticVersionRegEx(major, minor, point) => Some((major, minor, point))
                case _ => None
              }
            }
            case None => None //we might fail if the version attributes looks different than what we are expected to. perhaps better?
          }
          Some(requirement.copy(constraints = constraints))
        } else Some(requirement)
      }.toSet
      configuration.copy(requirements = requirements)
    }

  }

}