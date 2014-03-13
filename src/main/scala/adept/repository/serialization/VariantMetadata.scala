package adept.repository.serialization

import adept.repository.models._
import adept.resolution.models._
import adept.repository.Repository
import play.api.libs.json.Format
import adept.artifact.models.ArtifactRef
import java.io.FileInputStream
import java.io.FileOutputStream
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.io.BufferedWriter
import java.io.FileWriter
import adept.utils.Hasher
import adept.repository.GitRepository
import java.io.File

case class VariantMetadata(attributes: Seq[Attribute], artifacts: Seq[ArtifactRef], requirements: Seq[Requirement]) {

  def toVariant(id: Id): Variant = {
    Variant(id, attributes.toSet, artifacts.toSet, requirements.toSet)
  }

  lazy val hash: VariantHash = {
    VariantHash(Hasher.hash(jsonString.getBytes))
  }

  lazy val jsonString = Json.prettyPrint(Json.toJson(this))

  def write(id: Id, repository: Repository): File = {
    require(hash.value.length == Repository.HashLength, "Hash for: " + id + " (" + this + ") had length:" + hash.value.length + " but should have " + Repository.HashLength)
    val file = repository.getVariantFile(id, hash)
    MetadataContent.write(jsonString, file)
  }
}

object VariantMetadata {

  def fromVariant(variant: Variant): VariantMetadata = {
    VariantMetadata(variant.attributes.toSeq, variant.artifacts.toSeq, variant.requirements.toSeq)
  }

  import ArtifactMetadata._

  private[adept] implicit val requirementFormat: Format[Requirement] = {
    (
      (__ \ "id").format[String] and
      (__ \ "constraints").format[Map[String, Set[String]]])({
        case (id, constraints) =>
          Requirement(
            Id(id),
            constraints.map { case (name, values) => Constraint(name, values) }.toSet)
      },
        unlift({ r: Requirement =>
          val Requirement(id, constraints) = r
          Some((
            id.value,
            constraints.toSeq.sorted.map(c => c.name -> c.values).toMap))
        }))
  }

  private[adept] implicit def format: Format[VariantMetadata] = {
    (
      (__ \ "attributes").format[Map[String, Set[String]]] and
      (__ \ "artifacts").format[Seq[ArtifactRef]] and
      (__ \ "requirements").format[Seq[Requirement]])({
        case (attributes, artifacts, requirements) =>
          VariantMetadata(
            attributes.map { case (name, values) => Attribute(name, values) }.toSeq,
            artifacts,
            requirements)
      },
        unlift({ vm: VariantMetadata =>
          val VariantMetadata(attributes, artifacts, requirements) = vm
          Some((
            attributes.toSeq.sorted.map(a => a.name -> a.values).toMap,
            artifacts.toSeq.sorted,
            requirements.toSeq.sorted))
        }))
  }

  def read(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit): Option[Variant] = {
    repository.usingVariantInputStream(id, hash, commit) {
      case Right(Some(is)) =>
        val json = Json.parse(io.Source.fromInputStream(is).getLines.mkString("\n"))
        Json.fromJson[VariantMetadata](json) match {
          case JsSuccess(value, _) => Some(value.toVariant(id))
          case JsError(errors) => throw new Exception("Could parse json: " + hash + " for commit: " + commit + " in dir:  " + repository.dir + " ("+ repository.getVariantFile(id, hash).getAbsolutePath + "). Got errors: " + errors)
        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + hash + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }
}