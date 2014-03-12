package adept.repository.serialization

import adept.artifact.models._
import adept.repository.Repository
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.repository.models.Commit
import adept.repository.GitRepository
import java.io.File

case class ArtifactMetadata(size: Long, locations: Set[String]) {
  def toArtifact(hash: ArtifactHash): Artifact = {
    Artifact(hash, size, locations)
  }

  lazy val jsonString = Json.prettyPrint(Json.toJson(this))

  def write(hash: ArtifactHash, repository: Repository): File = {
    val file = repository.getArtifactFile(hash)
    MetadataContent.write(jsonString, file)
  }
}

object ArtifactMetadata {

  private[adept] implicit val formatArtifactRef: Format[ArtifactRef] = {
    (
      (__ \ "hash").format[String] and
      (__ \ "attributes").format[Map[String, Set[String]]] and
      (__ \ "filename").format[Option[String]])({
        case (hashString, attributes, filename) =>
          ArtifactRef(ArtifactHash(hashString),
            attributes.map { case (name, values) => ArtifactAttribute(name, values) }.toSet,
            filename)
      }, unlift({ a: ArtifactRef =>
        val ArtifactRef(hash, attributes, filename) = a
        Some((hash.value,
          attributes.toSeq.sorted.map(o => o.name -> o.values).toMap,
          filename))
      }))
  }

  private[adept] implicit val formatArtifactMetadata: Format[ArtifactMetadata] = {
    (
      (__ \ "size").format[Long] and
      (__ \ "locations").format[Set[String]])({
        case (size, locations) =>
          ArtifactMetadata(size, locations)
      }, unlift({ a: ArtifactMetadata =>
        val ArtifactMetadata(size, locations) = a
        Some((size, locations))
      }))
  }

  private[adept] def read(hash: ArtifactHash, repository: GitRepository, commit: Commit): Option[Artifact] = {
    repository.usingArtifactInputStream(hash, commit) {
      case Right(Some(is)) =>
        val json = Json.parse(io.Source.fromInputStream(is).getLines.mkString("\n"))
        Json.fromJson[ArtifactMetadata](json) match {
          case JsSuccess(value, _) => Some(value.toArtifact(hash))
          case JsError(errors) => throw new Exception("Could parse json: " + hash + " for commit: " + commit + " in dir:  " + repository.dir + " ("+ repository.getArtifactFile(hash).getAbsolutePath + "). Got errors: " + errors)
        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + hash + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }
}