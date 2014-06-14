package adept.repository.metadata

import adept.repository.models.RepositoryName
import adept.repository.models.RepositoryLocations
import adept.repository.Repository
import java.io.{InputStream, File}
import adept.repository.GitRepository
import adept.repository.models.Commit
import adept.services.JsonService
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}

case class RepositoryLocationsMetadata(uris: Seq[String]) {
  def toRepositoryLocations(name: RepositoryName): RepositoryLocations = {
    RepositoryLocations(name, uris.toSet)
  }

  lazy val jsonString = {
    JsonService.writeJson({generator: JsonGenerator =>
      JsonService.writeStringArrayField("uris", uris.sorted, generator)
    })
  }

  def write(name: RepositoryName, repository: Repository): File = { //name: the name of the repository where the uris are pointing. repository represents the directory where you are storing this information
    val file = repository.ensureRepositoryLocationsFile(name)
    MetadataContent.write(jsonString, file)
  }
}

object RepositoryLocationsMetadata {
  import Repository._
  import GitRepository._

  private[adept] val LocationsRegex = {
    s"""$RepositoryLocationsMetadataDirName$GitPathSep(.*?)$GitPathSep$VariantMetadataFileName""".r
  }

  def listLocations(repository: GitRepository, commit: Commit): Set[RepositoryLocationsMetadata] = {
    repository.usePath[RepositoryLocationsMetadata](Some(Repository.RepositoryLocationsMetadataDirName), commit) { path =>
      path match {
        case LocationsRegex(name) =>
          RepositoryLocationsMetadata.read(RepositoryName(name), repository, commit)
        case _ => None
      }
    }
  }

  def read(name: RepositoryName, repository: GitRepository, commit: Commit): Option[RepositoryLocationsMetadata] = {
    repository.usingRepositoryLocationsStream(name, commit) {
      case Right(Some(is)) =>
        readJson(is)
//        Json.fromJson[Seq[String]](json) match {
//          case JsSuccess(values, _) => Some(RepositoryLocationsMetadata(values))
//          case JsError(errors) => throw new Exception("Could parse json for repository locations in: " + name + " for commit: " + commit + " in dir:  " + repository.dir + " (" + repository.getRepositoryLocationsFile(name).getAbsolutePath + "). Got errors: " + errors)
//        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + name + " for commit: " + commit + " in dir:  " +
          repository.dir + ". Got error: " + error)
    }
  }

  def read(name: RepositoryName, repository: Repository): Option[RepositoryLocationsMetadata] = {
    val file = repository.getRepositoryLocationsFile(name)
    repository.usingFileInputStream(repository.getRepositoryLocationsFile(name)) {
      case Right(Some(is)) =>
        readJson(is)
//        Json.fromJson[Seq[String]](json) match {
//          case JsSuccess(values, _) => Some(RepositoryLocationsMetadata(values))
//          case JsError(errors) => throw new Exception("Could parse json for repository locations in: " + name + " in dir:  " + repository.dir + " (" + file.getAbsolutePath + "). Got errors: " + errors)
//        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + name + " for file: " + file.getAbsolutePath() + " in dir:  " +
          repository.dir + ". Got error: " + error)
    }
  }

  private def readJson(is: InputStream): Option[RepositoryLocationsMetadata] = {
    var uris: Option[Seq[String]] = null
    val json = JsonService.parseJson(is, (parser: JsonParser, fieldName: String) => {
      fieldName match {
        case "uris" =>
          uris = Some(JsonService.parseStringSeq(parser))
      }
    })
    if (!uris.isDefined) {
      throw new Exception(s"Invalid JSON: $json")
    }

    Some(RepositoryLocationsMetadata(uris.get))
  }
}
