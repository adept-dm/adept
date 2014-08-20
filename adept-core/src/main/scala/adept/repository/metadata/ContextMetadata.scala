package adept.repository.metadata

import java.io.{File, InputStream}

import adept.repository.{GitRepository, Repository}
import adept.repository.models._
import adept.resolution.models._
import adept.services.JsonService
import com.fasterxml.jackson.core.JsonGenerator

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
        throw new Exception("Could not read: " + id + "#" + hash + " in dir:  " + repository.dir +
          ". Got error: " + error)
    }
  }

  def read(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit):
  Option[ContextMetadata] = {
    repository.usingContextInputStream(id, hash, commit) {
      case Right(Some(is)) =>
        readJson(is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + id + "#" + hash + " for commit: " + commit +
          " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }

  def readJson(is: InputStream): Option[ContextMetadata] = {
    JsonService.parseJson(is, Map(
      ("values", JsonService.parseSeq(_, ContextValue.fromJson))
    ), valueMap => Some(ContextMetadata(valueMap.getSeq[ContextValue]("values"))))._1
  }
}
