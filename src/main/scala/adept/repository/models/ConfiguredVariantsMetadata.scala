package adept.repository.models

import adept.models._
import adept.repository.models.configuration._
import java.io.Reader
import java.io.Writer
import java.io.BufferedReader
import java.io.BufferedWriter
import adept.repository.models.serialization.DeserializationException

object ConfiguredVariantsMetadata {
  import play.api.libs.json._
  import play.api.libs.json.Json

  private def readString(reader: Reader) = {
    val bufferedReader = new BufferedReader(reader)
    try {
      var lines = new StringBuilder
      var line = bufferedReader.readLine()
      while (line != null) {
        lines.append(line)
        line = bufferedReader.readLine()
      }
      lines.toString
    } finally {
      bufferedReader.close()
    }
  }

  def fromJson(reader: Reader): ConfiguredVariantsMetadata = {
    import play.api.libs.json._
    import adept.repository.models.serialization.AdeptFormats._
    Json.parse(readString(reader)).validate[ConfiguredVariantsMetadata].fold(
      errors => throw new DeserializationException(errors
        .map {
          case (jsPath, validationErrors) =>
            "Failed at: " + jsPath.toString + " with " + validationErrors.mkString(" and ")
        }),
      value => value)
  }
}

case class ConfiguredVariantsMetadata(id: Id, metadata: Set[MetadataInfo], attributes: Set[Attribute], configurations: Set[Configuration]) {

  def toVariants(): Set[(Variant, Set[RepositoryMetadata])] = {
    configurations.map { configuration =>
      val variantId = ConfigurationId.join(id, configuration.id)
      val variantAttributes = attributes ++ configuration.attributes
      val variantArtifacts = configuration.artifacts
      var repositories = Set.empty[RepositoryMetadata] //easier to read than a fold

      val variantRequirements = configuration.requirements.flatMap { configuredRequirement =>
        configuredRequirement.configurations.map { requirementConfig =>
          val variantRequirementId = ConfigurationId.join(configuredRequirement.id, requirementConfig)
          repositories += configuredRequirement.commit //MUTATE!
          Requirement(variantRequirementId, configuredRequirement.constraints)
        }
      }

      Variant(variantId, variantAttributes, variantArtifacts, variantRequirements) -> repositories
    }
  }

  private def writeString(writer: Writer, lines: String) = {
    val bufferedWriter = new BufferedWriter(writer)
    try {
      bufferedWriter.write(lines)
      bufferedWriter.flush()
    } finally {
      bufferedWriter.close()
    }
  }

  def toJson(writer: Writer) = {
    import play.api.libs.json._
    import play.api.libs.json.Json
    import adept.repository.models.serialization.AdeptFormats._

    val content = Json.prettyPrint(Json.toJson(this))
    writeString(writer, content)
  }
}