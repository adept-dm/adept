package adept.repository.metadata

import adept.repository.models._
import adept.resolution.models._
import adept.repository.Repository
import play.api.libs.json._
import play.api.libs.functional.syntax._
import java.io.BufferedWriter
import java.io.FileWriter
import adept.repository.GitRepository
import java.io.File

case class ResolutionResultsMetadata(values: Seq[ResolutionResult]) {
  import ResolutionResultsMetadata._
  lazy val jsonString = Json.prettyPrint(Json.toJson(values.sorted))

  def write(id: Id, hash: VariantHash, repository: Repository): File = {
    val file = repository.ensureResolutionResultsFile(id, hash)
    MetadataContent.write(jsonString, file)
  }
}

object ResolutionResultsMetadata {

  private[adept] implicit def format: Format[ResolutionResult] = {
    (
      (__ \ "id").format[String] and
      (__ \ "repository").format[String] and
      (__ \ "commit").format[Option[String]] and
      (__ \ "variant").format[String])({
        case (id, repository, commit, variant) =>
          ResolutionResult(
            Id(id),
            RepositoryName(repository),
            commit.map(Commit(_)),
            VariantHash(variant))
      },
        unlift({ r: ResolutionResult =>
          val ResolutionResult(id, repository, commit, variant) = r
          Some((
            id.value,
            repository.value,
            commit.map(_.value),
            variant.value))
        }))
  }

  def read(id: Id, hash: VariantHash, repository: Repository): Option[ResolutionResultsMetadata] = {
    val file = repository.getResolutionResultsFile(id, hash)
    repository.usingFileInputStream(file) {
      case Right(Some(is)) =>
        val json = Json.parse(io.Source.fromInputStream(is).getLines.mkString("\n"))
        Json.fromJson[Seq[ResolutionResult]](json) match {
          case JsSuccess(values, _) => Some(ResolutionResultsMetadata(values))
          case JsError(errors) => throw new Exception("Could parse json: " + id + "#" + hash + " in dir:  " + repository.dir + " (" + file.getAbsolutePath() + "). Got errors: " + errors)
        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + id + "#" + hash + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }

  def read(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit): Option[ResolutionResultsMetadata] = {
    repository.usingResolutionResultsInputStream(id, hash, commit) {
      case Right(Some(is)) =>
        val json = Json.parse(io.Source.fromInputStream(is).getLines.mkString("\n"))
        Json.fromJson[Seq[ResolutionResult]](json) match {
          case JsSuccess(values, _) => Some(ResolutionResultsMetadata(values))
          case JsError(errors) => throw new Exception("Could parse json: " + id + "#" + hash + " for commit: " + commit + " in dir:  " + repository.dir + " (" + repository.asGitPath(repository.getResolutionResultsFile(id, hash)) + "). Got errors: " + errors)
        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + id + "#" + hash + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }
}