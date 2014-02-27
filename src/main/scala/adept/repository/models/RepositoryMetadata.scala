package adept.repository.models

import adept.models.Hash
import java.io.Reader
import java.io.File
import adept.models.Artifact
import java.io.Writer
import adept.models.Id
import adept.repository.models.configuration.ConfigurationId
import adept.repository.AdeptCommit
import adept.repository.AdeptGitRepository

object RepositoryMetadata {

  def fromJson[A](reader: Reader) = {
    import adept.repository.models.serialization.AdeptFormats._
    MetadataContent.fromJson(reader)(jsValue => jsValue.validate[RepositoryMetadata])
  }

  def file(repository: AdeptGitRepository, id: Id, variants: Set[Hash]): File = {
    repository.getRepositoryMetadataFile(id, variants)
  }

}


case class RepositoryInfo(id: Id, repository: String, commit: Commit)
case class RepositoryConfiguration(id: ConfigurationId, info: Seq[RepositoryInfo])

/**
 * Information that is needed load repositories.
 */
case class RepositoryMetadata(id: Id, variants: Set[Hash], configurations: Seq[RepositoryConfiguration]) {
  def load(baseDir: File, id: Id, configuration: ConfigurationId): Set[AdeptCommit] = { //TODO: this will be replaced with something smarter that actually loads repositories. it should probably not be in this file either btw
    configurations.filter(_.id == configuration).flatMap{ repoConf =>
      repoConf.info.map{ repoInfo =>
        AdeptCommit(new AdeptGitRepository(baseDir, repoInfo.repository), repoInfo.commit)
      }
    }.toSet
  }
  
  def toJson(writer: Writer) = {
    import play.api.libs.json.Json
    import adept.repository.models.serialization.AdeptFormats._
    val content = Json.prettyPrint(Json.toJson(this))
    MetadataContent.writeString(writer, content)
  }

  def write(repository: AdeptGitRepository): File = {
    MetadataContent.usingFileWriter(RepositoryMetadata.file(repository, id, variants))(toJson)
  }
}