package adept.repository.models

import adept.models.Hash
import java.io.Reader
import adept.repository.AdeptGitRepository
import java.io.File
import adept.models.Artifact
import java.io.Writer

object ArtifactMetadata {

  def fromJson[A](reader: Reader) = {
    import adept.repository.models.serialization.AdeptFormats._
    MetadataContent.fromJson(reader)(jsValue => jsValue.validate[ArtifactMetadata])
  }

  def fromArtifact(artifact: Artifact) = ArtifactMetadata(artifact.hash, artifact.size, artifact.locations)

  def file(repository: AdeptGitRepository, hash: Hash): File = {
    repository.getArtifactMetadataFile(hash)
  }
}

//TODO: should we remove this class? It is exactlyt the same as Artifact, but the idea was that artifact representation might change from what is stored.
case class ArtifactMetadata(hash: Hash, size: Long, locations: Set[String]) {
  def toArtifact = Artifact(hash, size, locations)

  def toJson(writer: Writer) = {
    import play.api.libs.json.Json
    import adept.repository.models.serialization.AdeptFormats._
    val content = Json.prettyPrint(Json.toJson(this))
    MetadataContent.writeString(writer, content)
  }

  def write(repository: AdeptGitRepository): File = {
    MetadataContent.usingFileWriter(ArtifactMetadata.file(repository, hash))(toJson)
  }
}
