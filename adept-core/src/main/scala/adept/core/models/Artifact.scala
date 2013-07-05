package adept.core.models

import java.io.File

case class Artifact(hash: Hash, artifactType: String, configurations: Set[String], locations: Set[String])

object Artifact {
  import org.json4s._
  import adept.utils.JsonHelpers._
  import org.json4s.JsonDSL._
  
  def readArtifact(json: JValue): Either[String, Artifact] = {
    val maybeLocations = getOptionalStrings(json, "locations")

    val maybeConfigurations = getOptionalStrings(json, "configurations")

    for {
      hash <- (eitherOf[String](json) \ "hash").right
      artifactType <- (eitherOf[String](json) \ "artifact-type").right
      locations <- maybeLocations.right
      configurations <- maybeConfigurations.right
    } yield {
      Artifact(Hash(hash), artifactType, configurations, locations)
    }
  }

  def readArtifacts(json: JValue): Either[String, Set[Artifact]] = {
    readSet(json) { f =>
      f.map(readArtifact)
    }
  }

  def artifactToJson(artifact: Artifact): JValue = {
    asJObject(List[JField](
      ("hash" -> artifact.hash.value),
      ("artifact-type" -> artifact.artifactType),
      ("configurations" -> artifact.configurations),
      ("locations" -> artifact.locations)).map(ifNonEmpty): _*)
  }

  def fromFile(file: File, artifactType: String, configurations: Set[String], locations: Set[String]) = {
    Artifact(Hash.calculate(file), artifactType, configurations, locations)
  }
}
