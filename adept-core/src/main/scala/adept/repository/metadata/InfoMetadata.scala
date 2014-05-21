package adept.repository.metadata

import adept.artifact.models._
import adept.repository.Repository
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.repository.models.Commit
import adept.repository.GitRepository
import java.io.File
import collection.JavaConverters._
import adept.resolution.models.ArtifactRef
import java.io.InputStream
import adept.resolution.models._
import adept.repository.models._
import java.util.Date
import java.text.SimpleDateFormat
import java.text.DateFormat

case class LicenseInfo(name: Option[String], url: Option[String])
case class VcsInfo(url: Option[String], connection: Option[String])

@deprecated("InfoMetadata might change format on release")
case class InfoMetadata(description: Option[String], homePage: Option[String], publicationDate: Option[Date], vcs: Option[VcsInfo], licenses: Seq[LicenseInfo], other: Map[String, Seq[String]] = Map.empty) {
  lazy val jsonString = Json.prettyPrint(Json.toJson(this))

  def write(id: Id, hash: VariantHash, repository: Repository): File = {
    val file = repository.ensureInfoFile(id, hash)
    MetadataContent.write(jsonString, file)
  }
}

object InfoMetadata {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

  private[repository] implicit val format: Format[InfoMetadata] = {
    (
      (__ \ "description").format[Option[String]] and
      (__ \ "homepage").format[Option[String]] and
      (__ \ "publication-date").format[Option[String]] and
      (__ \ "licenses").format[Seq[Map[String, String]]] and
      (__ \ "vcs").format[Option[Map[String, String]]] and
      (__ \ "other").format[Map[String, Seq[String]]])({
        case (description, homePage, publicationDate, licensesRaw, vcsRaw, other) =>
          val vcs = vcsRaw.map { vcsRaw =>
            VcsInfo(vcsRaw.get("url"), vcsRaw.get("connection"))
          }
          val licenses = licensesRaw.map { licenseRaw =>
            LicenseInfo(licenseRaw.get("name"), licenseRaw.get("url"))
          }
          InfoMetadata(description, homePage, publicationDate.map(dateFormat.parse), vcs, licenses, other)
      },
        unlift({ im: InfoMetadata =>
          val InfoMetadata(description, homePage, publicationDate, vcs, licenses, other) = im
          Some((description, homePage, publicationDate.map(dateFormat.format), licenses.map { license =>
            Map("name" -> license.name.getOrElse(""), "url" -> license.url.getOrElse(""))
          }, vcs.map { vcs =>
            Map("url" -> vcs.url.getOrElse(""), "connection" -> vcs.connection.getOrElse(""))
          }, other))
        }))
  }

  def read(id: Id, hash: VariantHash, repository: Repository): Option[InfoMetadata] = {
    val file = repository.getInfoFile(id, hash)
    repository.usingFileInputStream(file) {
      case Right(Some(is)) =>
        readJson(id, hash, repository, is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read file: " + file.getAbsolutePath + " for hash: " + hash + ". Got error: " + error)
    }
  }

  private def readJson(id: Id, hash: VariantHash, repository: Repository, is: InputStream) = {
    val json = Json.parse(io.Source.fromInputStream(is).getLines.mkString("\n"))
    Json.fromJson[InfoMetadata](json) match {
      case JsSuccess(value, _) => Some(value)
      case JsError(errors) => throw new Exception("Could parse json: " + hash + " in dir:  " + repository.dir + " (" + repository.getInfoFile(id, hash).getAbsolutePath + "). Got errors: " + errors)
    }
  }

  def read(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit): Option[InfoMetadata] = {
    repository.usingInfoInputStream(id, hash, commit) {
      case Right(Some(is)) =>
        readJson(id, hash, repository, is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + hash + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }
}