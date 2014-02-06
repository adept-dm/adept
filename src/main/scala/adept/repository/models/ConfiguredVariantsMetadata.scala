package adept.repository.models

import adept.models._
import adept.repository.models.configuration._
import java.io.Reader
import java.io.Writer
import java.io.BufferedReader
import java.io.BufferedWriter
import adept.repository.models.serialization.DeserializationException
import adept.repository.AdeptGitRepository
import java.io.File
import java.io.FileWriter

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

//TODO: nicer name?
case class ConfiguredVariantsMetadata(id: Id, metadata: Set[MetadataInfo], attributes: Set[Attribute], configurations: Set[Configuration]) {
  override lazy val toString = {
    id + " " + attributes.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("[", ",", "]")
  }

  lazy val hash = {
    //FIXME: move to Hash? Is this safe?
    var strings = Set.empty[String]
    configurations.foreach { configuration =>
      strings ++= configuration.attributes.map(_.toString)
      strings += configuration.id.value
      strings ++= configuration.requirements.map(_.toString)
    }
    val md = Hash.md.get

    strings.toSeq.sorted.foreach { string =>
      md.update(string.getBytes)
    }
    Hash(md.digest().map(b => "%02x" format b).mkString)
  }

  def toVariants(repositoryName: String): Set[(Variant, Set[RepositoryMetadata])] = {
    val id = Id(repositoryName + Id.Sep + this.id.value)
    
    //This Variant is needed to make sure there never different variants in different configurations with the same 'base' Id that is correctly resolved
    val baseVariant = Variant(id, attributes = attributes + Attribute(Configuration.ConfigurationHashAttributeName, Set(hash.value)))

    configurations.map { configuration =>
      val variantId = ConfigurationId.join(id, configuration.id)
      val variantAttributes = attributes ++ configuration.attributes
      val variantArtifacts = configuration.artifacts
      var repositories = Set.empty[RepositoryMetadata] //easier to read than a fold

      val variantRequirements = configuration.requirements.flatMap { configuredRequirement =>
          repositories += configuredRequirement.commit //MUTATE!
          configuredRequirement.asRequirements
      } + Requirement(id, Set(Constraint(Configuration.ConfigurationHashAttributeName, Set(hash.value))))

      Variant(variantId, variantAttributes, variantArtifacts, variantRequirements) -> repositories
    } + (baseVariant -> Set.empty)
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

  def file(repository: AdeptGitRepository): File = {
    repository.getVariantsMetadataFile(id, hash)
  }

  def write(repository: AdeptGitRepository): File = {
    val currentFile = file(repository)
    val writer = new FileWriter(currentFile)
    try {
      toJson(writer)
      currentFile
    } finally {
      writer.close()
    }
  }
}