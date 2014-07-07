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
  def writeJson(generator: JsonGenerator): Unit = {
    JsonService.writeStringField("name", name, generator)
    JsonService.writeStringField("url", url, generator)
  }
}

object LicenseInfo {
  def fromJson(parser: JsonParser): LicenseInfo = {
    JsonService.parseObject(parser, Map(
        ("name", _.getValueAsString),
        ("url", _.getValueAsString)
      ), valueMap => LicenseInfo(valueMap.getOption[String]("name"),
      valueMap.getOption[String]("url")))
  }
}

case class VcsInfo(url: Option[String], connection: Option[String]) extends JsonSerializable {
  def writeJson(generator: JsonGenerator): Unit = {
    JsonService.writeStringField("url", url, generator)
    JsonService.writeStringField("connection", connection, generator)
  }
}

object VcsInfo {
  def fromJson(parser: JsonParser): VcsInfo = {
    JsonService.parseObject(parser, Map(
      ("url", _.getValueAsString),
      ("connection", _.getValueAsString)
    ),
    valueMap => VcsInfo(valueMap.getOption[String]("url"),
      valueMap.getOption[String]("connection")))
  }
}

@deprecated("InfoMetadata might change format on release")
case class InfoMetadata(description: Option[String], homePage: Option[String], publicationDate:
Option[Date],
                        vcs: Option[VcsInfo], licenses: Seq[LicenseInfo],
                        other: Map[String, Seq[String]] = Map.empty) {
  lazy val jsonString = {
    JsonService.writeJson({generator: JsonGenerator =>
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
        throw new Exception("Could not read file: " + file.getAbsolutePath + " for hash: " + hash +
          ". Got error: " + error)
    }
  }

  private def readJson(id: Id, hash: VariantHash, repository: Repository, is: InputStream):
  Option[InfoMetadata] = {
    JsonService.parseJson(is, Map(
        ("description", _.getValueAsString),
        ("homePage", _.getValueAsString),
        ("publicationDate", JsonService.parseDate(_, dateFormat)),
        ("vcs", VcsInfo.fromJson(_)),
        ("licenses", JsonService.parseSeq(_, LicenseInfo.fromJson)),
        ("other", JsonService.parseStringSeqMap(_))
      ), (valueMap) => Some(InfoMetadata(valueMap.getOption[String]("description"),
        valueMap.getOption[String]("homePage"), valueMap.getOption[Date]("publicationDate"),
        valueMap.getOption[VcsInfo]("vcs"), valueMap.getSeq[LicenseInfo]("licenses"),
        valueMap.getStringSeqMap("other")))
    )._1
  }

  def read(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit): Option[InfoMetadata] = {
    repository.usingInfoInputStream(id, hash, commit) {
      case Right(Some(is)) =>
        readJson(id, hash, repository, is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + hash + " for commit: " + commit + " in dir:  " +
          repository.dir + ". Got error: " + error)
    }
  }
}
