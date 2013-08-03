package adept.core.models

import java.io.File
import java.net.URL
import adept.download.Downloader
import akka.util.FiniteDuration
import akka.actor._
import adept.download.ProgressIndicator
import adept.utils.Logging
import akka.dispatch.Await

case class Artifact(hash: Hash, artifactType: String, configurations: Set[String], locations: Set[String])

object Artifact extends Logging {
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

  //TODO: move this logic out from models
  def fromUrl(url: URL, artifactType: String, configurations: Set[String], extraLocations: Set[String] = Set.empty)(timeout: FiniteDuration) = {
    val file = File.createTempFile("adept-artifact", "." + artifactType)
    val system = ActorSystem("adept-artifact-download")
    val progressActor = system.actorOf(Props[ProgressIndicator])
    val future = Downloader.downloadOne(url, file, None)(timeout, system).map {
      case Right(file) =>
        Artifact(Hash.calculate(file), artifactType, configurations, extraLocations + url.toString)
      case Left(exception) =>
        logger.error("could not download from: " + url)
        throw exception
    }

    try {
      Await.result(future, timeout)
    } finally {
      file.delete()
    }
  }
}
