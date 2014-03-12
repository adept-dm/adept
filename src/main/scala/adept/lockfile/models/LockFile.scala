package adept.lockfile.models

import adept.resolution.models._
//}
//
//case class LockFileArtifact(hash: Hash, size: Long, locations: Set[String], filename: Option[String])
//
//case class LockFile(hash: Hash, requirements: Seq[LockFileRequirement], artifacts: Seq[LockFileArtifact]) {
//  def write(file: File) = {
//    import adept.repository.models.serialization.AdeptFormats._
//    val content = Json.prettyPrint(Json.toJson(this))
//    MetadataContent.usingFileWriter(file) { writer =>
//      MetadataContent.writeString(writer, content)
//    }
//  }
//}
//
//object LockFile {
//  def read(file: File) = {
//    import adept.repository.models.serialization.AdeptFormats._
//    //TODO: error handling? if (file.exists() && file.isFile) 
//    val source = io.Source.fromFile(file)
//    Json.parse(source.getLines.mkString("\n")).as[LockFile]
//  }
//}