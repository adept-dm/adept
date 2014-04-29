package adept.lockfile

import adept.artifact.models.ArtifactHash
import adept.artifact.models.Artifact
import adept.artifact.models.ArtifactRef
import adept.artifact.models.ArtifactAttribute
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class LockfileArtifact(hash: ArtifactHash, size: Long, locations: Seq[String], attributes: Seq[ArtifactAttribute], filename: Option[String]) {
  def toArtifact = Artifact(hash, size, locations.toSet)
  def toArtifactRef = ArtifactRef(hash, attributes.toSet, filename)
}

object LockfileArtifact {
  private[adept] implicit val formatLockfileArtifact: Format[LockfileArtifact] = {
    (
      (__ \ "hash").format[String] and
      (__ \ "size").format[Long] and
      (__ \ "locations").format[Seq[String]] and
      (__ \ "attributes").format[Map[String, Set[String]]] and
      (__ \ "filename").format[Option[String]])({
        case (hash, size, locations, attributes, filename) =>
          LockfileArtifact(ArtifactHash(hash), size, locations, attributes.map { case (name, values) => ArtifactAttribute(name, values) }.toSeq, filename)
      }, unlift({ a: LockfileArtifact =>
        val LockfileArtifact(hash, size, locations, attributes, filename) = a
        Some((hash.value, size, locations, attributes.map(a => a.name -> a.values).toMap, filename))
      }))
  }
}