package adept.ext.conversions

import adept.repository.models.VariantMetadata
import adept.models._
import adept.ext.AttributeDefaults

object ScalaBinaryVersion extends Conversion {

  private lazy val ScalaLibBinaryVersionRegEx = """(.*)_(\d\.\d{2})$""".r
  val ScalaLibId = Id("scala-library")

  def convertId(name: String) = name match {
    case ScalaLibBinaryVersionRegEx(newId, _) => Id(newId)
    case _ => Id(name)
  }
  
  def convert(variantMetadata: VariantMetadata, others: Set[VariantMetadata]): Option[VariantMetadata] = {
    (variantMetadata.id.value match { //fix Id and change scala requirement with binary version
      case ScalaLibBinaryVersionRegEx(newId, scalaBinaryVersion) => {
        val configurations = variantMetadata.configurations.map { configuration =>
          val requirements = configuration.requirements.map { requirement => 
            if (requirement.id == ScalaLibId && requirement.constraints.find(_.name == AttributeDefaults.BinaryVersionAttribute).isEmpty) { //if scala lib and binary version is not set
              requirement.copy(constraints = requirement.constraints + Constraint(AttributeDefaults.BinaryVersionAttribute, Set(scalaBinaryVersion)))
            } else requirement
          }
          configuration.copy(requirements = requirements)
        }
        Some(variantMetadata.copy( id = Id(newId), configurations = configurations ))
      }
      case _ => Some(variantMetadata)
    }).map{ configuredVariant => //rewrite all requirement ids (foo_2.10 => foo)
      val configurations = configuredVariant.configurations.map{ configuration =>
        val requirements = configuration.requirements.map{ requirement =>
          requirement.copy( id = convertId(requirement.id.value) )
        }
        configuration.copy(requirements = requirements)
      }
      configuredVariant.copy( configurations = configurations )
    }
  }
}