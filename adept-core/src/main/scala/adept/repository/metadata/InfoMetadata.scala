package adept.repository.metadata

import adept.repository.Repository
import adept.repository.GitRepository
import java.io.File
import java.io.InputStream
import adept.resolution.models._
import adept.repository.models._
import java.util.Date
import java.text.SimpleDateFormat
import adept.services.JsonService
import com.fasterxml.jackson.core.{JsonParser, JsonGenerator}
import adept.artifact.models.JsonSerializable

case class LicenseInfo(name: Option[String], url: Option[String]) extends JsonSerializable {
  def writeJson(generator: JsonGenerator) {
    JsonService.writeStringField("name", name, generator)
    JsonService.writeStringField("url", url, generator)
  }
}

object LicenseInfo {
  def fromJson(parser: JsonParser): LicenseInfo = {
    var name: Option[String] = null
    var url: Option[String] = null
    JsonService.parseObject(parser, (parser: JsonParser, fieldName: String) => {
      fieldName match {
        case "name" => name = Some(parser.getValueAsString())
        case "url" => url = Some(parser.getValueAsString())
      }
    })
    LicenseInfo(name, url)
  }
}

case class VcsInfo(url: Option[String], connection: Option[String]) extends JsonSerializable {
  def writeJson(generator: JsonGenerator) {
    JsonService.writeStringField("url", url, generator)
    JsonService.writeStringField("connection", connection, generator)
  }
}

object VcsInfo {
  def fromJson(parser: JsonParser): VcsInfo = {
    var url: Option[String] = null
    var connection: Option[String] = null
    JsonService.parseObject(parser, (parser: JsonParser, fieldName: String) => {
      fieldName match {
        case "url" => url = Some(parser.getValueAsString())
        case "connection" => connection = Some(parser.getValueAsString())
      }
    })
    VcsInfo(url, connection)
  }
}

@deprecated("InfoMetadata might change format on release")
case class InfoMetadata(description: Option[String], homePage: Option[String], publicationDate: Option[Date],
                        vcs: Option[VcsInfo], licenses: Seq[LicenseInfo],
                        other: Map[String, Seq[String]] = Map.empty) {
  lazy val jsonString = {
    JsonService.writeJson((generator: JsonGenerator) => {
      JsonService.writeStringField("description", description, generator)
      JsonService.writeStringField("homePage", homePage, generator)
      JsonService.writeStringField("publicationDate", publicationDate.map(
        InfoMetadata.dateFormat.format(_)), generator)
      JsonService.writeObject(vcs, generator)
      JsonService.writeArrayField("licenses", licenses, generator)
      JsonService.writeObjectField("other", other, generator)
    })
  }

  def write(id: Id, hash: VariantHash, repository: Repository): File = {
    val file = repository.ensureInfoFile(id, hash)
    MetadataContent.write(jsonString, file)
  }
}

object InfoMetadata {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

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

  private def readJson(id: Id, hash: VariantHash, repository: Repository, is: InputStream): Option[InfoMetadata] = {
    var description: Option[String] = null
    var homePage: Option[String] = null
    var publicationDate: Option[Date] = null
    var vcs: Option[VcsInfo] = null
    var licenses: Option[Seq[LicenseInfo]] = null
    var other: Option[Map[String, Seq[String]]] = null
    JsonService.parseJson(is, (parser: JsonParser, fieldName: String) => {
      fieldName match {
        case "description" =>
          description = Some(parser.getValueAsString())
        case "homePage" =>
          homePage = Some(parser.getValueAsString())
        case "publicationDate" =>
          publicationDate = Some(dateFormat.parse(parser.getValueAsString()))
        case "vcs" =>
          vcs = Some(VcsInfo.fromJson(parser))
        case "licenses" =>
          licenses = Some(JsonService.parseSeq(parser, () => {
            LicenseInfo.fromJson(parser)
          }))
        case "other" =>
          other = Some(JsonService.parseStringMap(parser))
      }
    })

    Some(InfoMetadata(description, homePage, publicationDate, vcs, licenses.get, other.get))

    //    Json.fromJson[InfoMetadata](json) match {
    //      case JsSuccess(value, _) => Some(value)
    //      case JsError(errors) => throw new Exception("Could parse json: " + hash + " in dir:  " + repository.dir + " (" + repository.getInfoFile(id, hash).getAbsolutePath + "). Got errors: " + errors)
    //    }
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
