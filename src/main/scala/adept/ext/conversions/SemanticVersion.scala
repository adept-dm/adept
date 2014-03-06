package adept.ext.conversions

import adept.repository.models.VariantMetadata
import adept.models._
import adept.ext.AttributeDefaults

object SemanticVersion {

  private lazy val SemanticVersionRegEx = """^(\d+)\.?(\d+)\.?(\\*|.+)$""".r

  def getSemanticVersion(attributes: Set[Attribute]) = {
    attributes.find(_.name == AttributeDefaults.VersionAttribute) match {
      case Some(Attribute(AttributeDefaults.VersionAttribute, values)) => values.headOption.flatMap { version =>
        version match {
          case SemanticVersionRegEx(major, minor, point) => Some((major, minor, point))
          case _ => None
        }
      }
      case None => throw new Exception("Expected to find a " + AttributeDefaults.VersionAttribute +  " in " + attributes + " but it did not match the pattern.")
    }
  }

}

class SemanticVersion(variantIds: Set[Id]) extends Conversion {
  import SemanticVersion._

  def convert(variantMetadata: VariantMetadata, others: Set[VariantMetadata]): Option[VariantMetadata] = {
    println("checking " + variantMetadata.id + " VS " + variantIds)
    if (variantIds(variantMetadata.id)) {
      val semanticVersion = getSemanticVersion(variantMetadata.attributes).filter { case (major, minor, point) => major.toInt > 0 } //we might fail if the version attributes looks different than what we are expected to. perhaps better?

      val attributes = semanticVersion match {
        case Some((major, minor, _)) => variantMetadata.attributes + Attribute(AttributeDefaults.BinaryVersionAttribute, Set(major + "." + minor))
        case _ => variantMetadata.attributes
      }

      val configurations = variantMetadata.configurations.map { configuration =>
        val requirements = configuration.requirements.flatMap { requirement =>
          if (variantIds(requirement.id)) { //TODO check repo names?

            val binaryConstraint = others.filter(_.id == requirement.id).flatMap(_.attributes).find(_.name == AttributeDefaults.VersionAttribute) match {
              case Some(Attribute(AttributeDefaults.VersionAttribute, values)) if values.size == 1 => values.headOption.flatMap { version =>
                version match {
                  //we might fail if the version constraints looks different than what we are expected to. perhaps better?
                  case SemanticVersionRegEx(major, minor, point) => Some(Constraint(AttributeDefaults.BinaryVersionAttribute, Set(major + "." + minor)))
                }
              }
              case None => {
                val constraints = others.flatMap { otherVariant =>
                  if (otherVariant.id == requirement.id) {
                    getSemanticVersion(otherVariant.attributes) match {
                      case Some((major, minor, point)) if major.toInt > 0 =>
                        Some(Constraint(AttributeDefaults.BinaryVersionAttribute, Set(major + "." + minor)))
                      case _ => None
                    }
                  } else None
                }.headOption
                constraints
              }
            }
            Some(requirement.copy(constraints = requirement.constraints ++ binaryConstraint))
          } else Some(requirement)
        }.toSet

        configuration.copy(requirements = requirements)
      }
      Some(variantMetadata.copy(attributes = attributes, configurations = configurations))
    } else Some(variantMetadata)
  }

}