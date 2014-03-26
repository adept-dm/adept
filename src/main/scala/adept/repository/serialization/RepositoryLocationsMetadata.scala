package adept.repository.serialization

import adept.repository.models.RepositoryName
import adept.repository.models.RepositoryLocations
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.repository.Repository
import java.io.File
import adept.repository.GitRepository
import adept.repository.models.Commit
import org.eclipse.jgit.lib.ConfigConstants

case class RepositoryLocationsMetadata(uris: Seq[String]) {
 def toRepositoryLocations(name: RepositoryName) : RepositoryLocations = {
    RepositoryLocations(name, uris.toSet)
  }

  lazy val jsonString = Json.prettyPrint(Json.toJson(uris.sorted))

  def write(name: RepositoryName, repository: Repository): File = { //name: the name of the repository where the uris are pointing. repository represents the directory where you are storing this information
    val file = repository.ensureRepositoryLocationsFile(name)
    MetadataContent.write(jsonString, file)
  }
}


object RepositoryLocationsMetadata {
  
  private[adept] def read(name: RepositoryName, repository: GitRepository, commit: Commit): Option[RepositoryLocationsMetadata] = {
    repository.usingRepositoryLocationsStream(name, commit) {
      case Right(Some(is)) =>
        val json = Json.parse(io.Source.fromInputStream(is).getLines.mkString("\n"))
        Json.fromJson[Seq[String]](json) match {
          case JsSuccess(values, _) => Some(RepositoryLocationsMetadata(values))
          case JsError(errors) => throw new Exception("Could parse json for repository locations in: " + name + " for commit: " + commit + " in dir:  " + repository.dir + " ("+ repository.getRepositoryLocationsFile(name).getAbsolutePath + "). Got errors: " + errors)
        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + name + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }
}