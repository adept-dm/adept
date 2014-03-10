package adept.repository.models

import adept.models._
import adept.repository.models.configuration._
import java.io.Reader
import java.io.Writer
import java.io.BufferedReader
import java.io.BufferedWriter
import adept.repository.models.serialization.DeserializationException
import adept.repository.Repository
import java.io.File
import java.io.FileWriter
import play.api.libs.json.Json

object VariantMetadata {
  import play.api.libs.json._
  import play.api.libs.json.Json

  def fromJson[A](reader: Reader) = {
    import adept.repository.models.serialization.AdeptFormats._

    MetadataContent.fromJson(reader)(jsValue => jsValue.validate[VariantMetadata])
  }

}

case class VariantMetadata(id: Id, metadataInfo: Set[MetadataInfo], attributes: Set[Attribute], configurations: Set[Configuration]) {
  override lazy val toString = {
    id + " " + attributes.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("[", ",", "]")
  }

  lazy val hash = {
    //FIXME: move to Hash? Is this safe?
    var strings = Seq.empty[String]
    strings +:= id.value
    metadataInfo.foreach { info =>
      strings +:= info.name
      strings ++:= info.values
    }
    attributes.foreach { a =>
      strings +:= a.name
      strings ++:= a.values
    }
    configurations.foreach { configuration =>
      configuration.attributes.foreach { a =>
        strings +:= a.name
        strings ++:= a.values
      }
      configuration.metadataInfo.foreach { info =>
        strings +:= info.name
        strings ++:= info.values
      }
      strings ++:= configuration.extendsConfigurations.map(_.value)
      configuration.artifacts.foreach { a =>
        a.attributes.foreach { a =>
          strings +:= a.name
          strings ++:= a.values
        }
        strings +:= a.hash.value
        strings +:= a.filename.getOrElse("")
      }
      strings +:= configuration.id.value
      configuration.requirements.foreach { req =>
        strings +:= req.id.value
        strings ++:= req.configurations.map(_.value)
        req.constraints.foreach { c =>
          strings +:= c.name
          strings ++:= c.values
        }
      }
    }
    val md = Hash.md.get

    strings.sorted.foreach { string =>
      md.update(string.getBytes)
    }
    Hash(md.digest().map(b => "%02x" format b).mkString)
  }

  def toVariants: Set[Variant] = { //TODO: here I rmeoved the repositoryname it feels a bit dangerous because you might have name clashes then again maybe that is solved somewhere else?
    //This Variant is needed to make sure there never different variants in different configurations with the same 'base' Id that is correctly resolved
    val baseVariant = Variant(id, attributes = attributes + Attribute(Configuration.ConfigurationHashAttributeName, Set(hash.value)))

    configurations.map { configuration =>
      val variantId = ConfigurationId.join(id, configuration.id)
      val variantAttributes = attributes ++ configuration.attributes
      val variantArtifacts = configuration.artifacts

      val variantRequirements = configuration.requirements.flatMap { configuredRequirement =>
        configuredRequirement.asRequirements
      } + Requirement(id, Set(Constraint(Configuration.ConfigurationHashAttributeName, Set(hash.value))))

      Variant(variantId, variantAttributes, variantArtifacts, variantRequirements)
    } + baseVariant
  }

  def toJson(writer: Writer) = {
    import play.api.libs.json.Json
    import adept.repository.models.serialization.AdeptFormats._
    val content = Json.prettyPrint(Json.toJson(this))
    MetadataContent.writeString(writer, content)
  }

  def file(repository: Repository): File = {
    repository.getVariantsMetadataFile(id, hash)
  }

  def write(repository: Repository): File = {
    MetadataContent.usingFileWriter(file(repository))(toJson)
  }
}