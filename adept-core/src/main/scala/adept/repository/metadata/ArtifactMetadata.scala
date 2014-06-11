package adept.repository.metadata

import adept.artifact.models._
import adept.repository.Repository
import com.fasterxml.jackson.databind.ObjectMapper
import adept.repository.models.Commit
import adept.repository.GitRepository
import java.io.File
import collection.JavaConverters._
import adept.resolution.models.ArtifactRef
import java.io.InputStream

case class ArtifactMetadata(size: Long, locations: Set[ArtifactLocation]) {
  def toArtifact(hash: ArtifactHash): Artifact = {
    new Artifact(hash, size, locations.asJava)
  }

  lazy val jsonString = toJson()

  def toJson() : String = {
    val mapper = new ObjectMapper()
    mapper.writeValueAsString(this)
  }

  def write(hash: ArtifactHash, repository: Repository): File = {
    val file = repository.ensureArtifactFile(hash)
    MetadataContent.write(jsonString, file)
  }
}

object ArtifactMetadata {

  def fromArtifact(artifact: Artifact): ArtifactMetadata = {
    ArtifactMetadata(artifact.size, Set() ++ artifact.locations.asScala)
  }

//  private[adept] implicit val formatArtifactRef: Format[ArtifactRef] = {
//    (
//      (__ \ "hash").format[String] and
//      (__ \ "attributes").format[Map[String, Set[String]]] and
//      (__ \ "filename").format[Option[String]])({
//        case (hashString, attributes, filename) =>
//          ArtifactRef(new ArtifactHash(hashString),
//            attributes.map { case (name, values) => new ArtifactAttribute(name, values.asJava) }.toSet,
//            filename)
//      }, unlift({ a: ArtifactRef =>
//        import ArtifactRef.orderingArtifactAttribute
//        val ArtifactRef(hash, attributes, filename) = a
//        Some((hash.value,
//          attributes.toSeq.sorted.map(o => o.name -> (Set() ++ o.values.asScala)).toMap,
//          filename))
//      }))
//  }
//
//  private[adept] implicit val formatArtifactMetadata: Format[ArtifactMetadata] = {
//    (
//      (__ \ "size").format[Long] and
//      (__ \ "locations").format[Set[String]])({
//        case (size, locations) =>
//          ArtifactMetadata(size, locations.map(new ArtifactLocation(_)))
//      }, unlift({ a: ArtifactMetadata =>
//        val ArtifactMetadata(size, locations) = a
//        Some((size, locations.map(_.value)))
//      }))
//  }

  def read(hash: ArtifactHash, repository: Repository): Option[ArtifactMetadata] = {
    val file = repository.getArtifactFile(hash)
    repository.usingFileInputStream(file) {
      case Right(Some(is)) =>
        readJson(hash, repository, is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read file: " + file.getAbsolutePath + " for hash: " + hash + ". Got error: " + error)
    }
  }

  private def readJson(hash: ArtifactHash, repository: Repository, is: InputStream) : Option[ArtifactMetadata] = {
    val mapper = new ObjectMapper()
    Some(mapper.readValue(io.Source.fromInputStream(is).getLines.mkString("\n"), classOf[ArtifactMetadata]))
//    Json.fromJson[ArtifactMetadata](json) match {
//      case JsSuccess(value, _) => Some(value)
//      case JsError(errors) => throw new Exception("Could not parse json: " + hash + " in dir:  " + repository.dir + " (" + repository.getArtifactFile(hash).getAbsolutePath + "). Got errors: " + errors)
//    }
  }

  def read(hash: ArtifactHash, repository: GitRepository, commit: Commit): Option[ArtifactMetadata] = {
    repository.usingArtifactInputStream(hash, commit) {
      case Right(Some(is)) =>
        readJson(hash, repository, is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + hash + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }
}
