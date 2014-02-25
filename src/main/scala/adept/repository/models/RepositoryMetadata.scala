package adept.repository.models

import adept.models.Hash
import java.io.Reader
import adept.repository.AdeptGitRepository
import java.io.File
import adept.models.Artifact
import java.io.Writer
import adept.models.Id
import adept.repository.models.configuration.ConfigurationId

object RepositoryMetadata {

  def fromJson[A](reader: Reader) = {
    import adept.repository.models.serialization.AdeptFormats._
    MetadataContent.fromJson(reader)(jsValue => jsValue.validate[ArtifactMetadata])
  }

  def file(repository: AdeptGitRepository, variant: Hash): File = {
    repository.getRepositoryMetadataFile(variant)
  }

}

case class RepositoryConfiguration(configuration: ConfigurationId, repositoryName: String, commit: Commit)

/**
 * Information that is needed load repositories.
 */
case class RepositoryMetadata(id: Id, variant: Hash, configurations: Seq[RepositoryConfiguration]) {
  def toJson(writer: Writer) = {
    import play.api.libs.json.Json
    import adept.repository.models.serialization.AdeptFormats._
    val content = Json.prettyPrint(Json.toJson(this))
    MetadataContent.writeString(writer, content)
  }

  def write(repository: AdeptGitRepository): File = {
    MetadataContent.usingFileWriter(RepositoryMetadata.file(repository, variant))(toJson)
  }
}