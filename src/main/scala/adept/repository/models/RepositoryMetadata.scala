package adept.repository.models

import adept.models.Hash
import java.io.Reader
import adept.repository.AdeptGitRepository
import java.io.File
import adept.models.Artifact
import java.io.Writer

object RepositoryMetadata {

  def fromJson[A](reader: Reader) = {
    import adept.repository.models.serialization.AdeptFormats._
    MetadataContent.fromJson(reader)(jsValue => jsValue.validate[ArtifactMetadata])
  }

  def fromArtifact(artifact: Artifact) = ArtifactMetadata(artifact.hash, artifact.size, artifact.locations)

  def file(repository: AdeptGitRepository, hash: Hash): File = {
    repository.getArtifactMetadataFile(hash)
  }

}

/**
 * Information that is needed load.
 * 
 * `info` can be used to make it easier to know which version this commit is pointing to.
 */
case class RepositoryMetadata(name: String, commit: Commit) {
  def toJson(writer: Writer) = {
    import play.api.libs.json.Json
    import adept.repository.models.serialization.AdeptFormats._
    val content = Json.prettyPrint(Json.toJson(this))
    MetadataContent.writeString(writer, content)
  }

  def write(repository: AdeptGitRepository): File = {
  //  MetadataContent.usingFileWriter(ArtifactMetadata.file(repository, hash))(toJson)
    ???
  }
}