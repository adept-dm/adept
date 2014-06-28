package adept.repository.metadata

import adept.artifact.models._
import adept.repository.Repository
import adept.repository.models.Commit
import adept.repository.GitRepository
import java.io.{File, InputStream}
import collection.JavaConverters._
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import adept.services.JsonService

case class ArtifactMetadata(size: Long, locations: Set[ArtifactLocation]) {
  def toArtifact(hash: ArtifactHash): Artifact = {
    new Artifact(hash, size, locations.asJava)
  }

  lazy val jsonString = JsonService.writeJson({ generator: JsonGenerator =>
    generator.writeNumberField("size", size)
    JsonService.writeStringArrayField("locations", locations.map(_.value), generator)
  })

  def write(hash: ArtifactHash, repository: Repository): File = {
    val file = repository.ensureArtifactFile(hash)
    MetadataContent.write(jsonString, file)
  }
}

object ArtifactMetadata {

  def fromArtifact(artifact: Artifact): ArtifactMetadata = {
    ArtifactMetadata(artifact.size, Set() ++ artifact.locations.asScala)
  }

  def read(hash: ArtifactHash, repository: Repository): Option[ArtifactMetadata] = {
    val file = repository.getArtifactFile(hash)
    repository.usingFileInputStream(file) {
      case Right(Some(is)) =>
        readJson(hash, repository, is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read file: " + file.getAbsolutePath + " for hash: " + hash +
          ". Got error: " + error)
    }
  }

  private def readJson(hash: ArtifactHash, repository: Repository, is: InputStream):
  Option[ArtifactMetadata] = {
    val (artifactMetadata, _) = JsonService.parseJson(is, Map(
      ("size", _.getValueAsLong),
      ("locations", JsonService.parseStringSet(_).map(new ArtifactLocation(_)))
    ), valueMap => ArtifactMetadata(valueMap.get[Long]("size"),
      valueMap.getSet[ArtifactLocation]("locations")))

    Some(artifactMetadata)
  }

  def read(hash: ArtifactHash, repository: GitRepository, commit: Commit):
  Option[ArtifactMetadata] = {
    repository.usingArtifactInputStream(hash, commit) {
      case Right(Some(is)) =>
        readJson(hash, repository, is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + hash + " for commit: " + commit + " in dir:  "
          + repository.dir + ". Got error: " + error)
    }
  }
}
