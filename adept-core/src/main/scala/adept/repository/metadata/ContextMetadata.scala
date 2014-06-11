package adept.repository.metadata

import adept.repository.models._
import adept.resolution.models._
import adept.repository.Repository
import com.fasterxml.jackson.databind.ObjectMapper
import java.io.BufferedWriter
import java.io.FileWriter
import adept.repository.GitRepository
import java.io.File

case class ContextMetadata(values: Seq[ContextValue]) {
  import ContextMetadata._
  lazy val jsonString = new ObjectMapper().writeValueAsString(values.sorted)

  def write(id: Id, hash: VariantHash, repository: Repository): File = {
    val file = repository.ensureContextFile(id, hash)
    MetadataContent.write(jsonString, file)
  }
}

object ContextMetadata {

//  private[adept] implicit def format: Format[ContextValue] = {
//    (
//      (__ \ "id").format[String] and
//      (__ \ "repository").format[String] and
//      (__ \ "commit").format[Option[String]] and
//      (__ \ "variant").format[String])({
//        case (id, repository, commit, variant) =>
//          ContextValue(
//            Id(id),
//            RepositoryName(repository),
//            commit.map(Commit(_)),
//            VariantHash(variant))
//      },
//        unlift({ cv: ContextValue =>
//          val ContextValue(id, repository, commit, variant) = cv
//          Some((
//            id.value,
//            repository.value,
//            commit.map(_.value),
//            variant.value))
//        }))
//  }

  def read(id: Id, hash: VariantHash, repository: Repository): Option[ContextMetadata] = {
    val file = repository.getContextFile(id, hash)
    repository.usingFileInputStream(file) {
      case Right(Some(is)) =>
        val mapper = new ObjectMapper()
        Some(mapper.readValue(io.Source.fromInputStream(is).getLines.mkString("\n"), classOf[ContextMetadata]))
//        Json.fromJson[Seq[ContextValue]](json) match {
//          case JsSuccess(values, _) => Some(ContextMetadata(values))
//          case JsError(errors) => throw new Exception("Could parse json: " + id + "#" + hash + " in dir:  " + repository.dir + " (" + file.getAbsolutePath() + "). Got errors: " + errors)
//        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + id + "#" + hash + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }

  def read(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit): Option[ContextMetadata] = {
    repository.usingContextInputStream(id, hash, commit) {
      case Right(Some(is)) =>
        Some(new ObjectMapper().readValue(io.Source.fromInputStream(is).getLines.mkString("\n"),
          classOf[ContextMetadata]))
//        Json.fromJson[Seq[ContextValue]](json) match {
//          case JsSuccess(values, _) => Some(ContextMetadata(values))
//          case JsError(errors) => throw new Exception("Could parse json: " + id + "#" + hash + " for commit: " + commit + " in dir:  " + repository.dir + " (" + repository.asGitPath(repository.getContextFile(id, hash)) + "). Got errors: " + errors)
//        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + id + "#" + hash + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }
}
