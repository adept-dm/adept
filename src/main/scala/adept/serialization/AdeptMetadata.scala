package adept.serialization

import adept.configuration._
import adept.core.models._
import java.io._
import org.json4s.{ Formats => Json4sFormats, Writer => _, Reader => _, _ }
import org.json4s.reflect.TypeInfo
import adept.repository.Commit

//TODO: split into files
case class Metadata(name: String, values: Set[String])

case class RepositoryMetadata(name: String, uris: Set[String], commit: Commit, info: String)

//TODO: configurations should be ConfigurationId, but I can't get it to deserialize correctly
case class Dependency(id: Id, configurations: Set[String], repositories: Set[RepositoryMetadata], constraints: Set[Constraint])

//FIXME: it is ugly to have `extends` but we avoid having to create a serializer this way. We must fix this prior to a beta
//also fix type of `extends` to be ConfigurationId
case class Configuration(id: ConfigurationId, `extends`: Set[String], metadata: Set[Metadata], artifacts: Set[ArtifactRef], attributes: Set[Attribute], dependencies: Set[Dependency])

object AdeptMetadata {
  def fromJson(reader: Reader): AdeptMetadata = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._
    
    read[AdeptMetadata](reader)
  }
}

case class AdeptMetadata(id: Id, metadata: Set[Metadata], attributes: Set[Attribute], configurations: Set[Configuration]) {
  def toVariants: Set[Variant] = {
    ???
  }

  def toJson(writer: Writer) = {

    import org.json4s.native.Serialization.writePretty
    import adept.serialization.Formats._

    writePretty(this, writer)
  }

  def hash: Hash = {
    ???
  }
}