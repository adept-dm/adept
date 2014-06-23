package adept.repository.metadata

import adept.repository.models._
import adept.resolution.models._
import adept.repository.Repository
import adept.repository.GitRepository
import java.io.{InputStream, File}
import adept.services.JsonService
import com.fasterxml.jackson.core.{JsonParser, JsonGenerator}

case class ContextMetadata(values: Seq[ContextValue]) {
  lazy val jsonString = {
    val json = JsonService.writeJson({generator: JsonGenerator =>
      JsonService.writeArrayField("values", values.sorted, generator)
    })
    json
  }

  def write(id: Id, hash: VariantHash, repository: Repository): File = {
    val file = repository.ensureContextFile(id, hash)
    MetadataContent.write(jsonString, file)
  }
}

object ContextMetadata {

  def read(id: Id, hash: VariantHash, repository: Repository): Option[ContextMetadata] = {
    val file = repository.getContextFile(id, hash)
    repository.usingFileInputStream(file) {
      case Right(Some(is)) =>
        readJson(is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + id + "#" + hash + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }

  def read(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit): Option[ContextMetadata] = {
    repository.usingContextInputStream(id, hash, commit) {
      case Right(Some(is)) =>
        readJson(is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + id + "#" + hash + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }

  private def readJson(is: InputStream): Option[ContextMetadata] = {
    var values: Option[Seq[ContextValue]] = null
    val json = JsonService.parseJson(is, (parser: JsonParser, fieldName: String) => {
      fieldName match {
        case "values" =>
          values = Some(JsonService.parseSeq(parser, () => {
            ContextValue.fromJson(parser)
          }))
      }
    })
    if (!values.isDefined) {
      throw new Exception(s"Invalid JSON: $json")
    }
    Some(ContextMetadata(values.get))
    //        Json.fromJson[Seq[ContextValue]](json) match {
    //          case JsSuccess(values, _) => Some(ContextMetadata(values))
    //          case JsError(errors) => throw new Exception("Could parse json: " + id + "#" + hash + " in dir:  " + repository.dir + " (" + file.getAbsolutePath() + "). Got errors: " + errors)
    //        }
  }
}
