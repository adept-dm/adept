package adept.repository.models

import adept.repository.models.configuration.ConfigurationId
import adept.models._
import java.io.File
import play.api.libs.json.Json
import adept.repository.models.configuration.ConfiguredRequirement
import adept.repository.AdeptCommit

//TODO: evaluate if Metadata and other files should all have Seq or Set. The argument for Seq is to make things more deterministic.
case class LockFileRequirement(id: Id, configuration: ConfigurationId, constraints: Seq[Constraint], repositoryName: String, repositoryCommit: Commit) {
  def asRequirement: Requirement = {
    Requirement(ConfigurationId.join(id, configuration), constraints.toSet)
  }
}

case class LockFileArtifact(hash: Hash, size: Long, locations: Set[String], filename: Option[String])

case class LockFile(hash: Hash, requirements: Seq[LockFileRequirement], artifacts: Seq[LockFileArtifact]) {
  def write(file: File) = {
    import adept.repository.models.serialization.AdeptFormats._
    val content = Json.prettyPrint(Json.toJson(this))
    MetadataContent.usingFileWriter(file) { writer =>
      MetadataContent.writeString(writer, content)
    }
  }
}

object LockFile {
  def read(file: File) = {
    import adept.repository.models.serialization.AdeptFormats._
    //TODO: error handling? if (file.exists() && file.isFile) 
    val source = io.Source.fromFile(file)
    Json.parse(source.getLines.mkString("\n")).as[LockFile]
  }
}