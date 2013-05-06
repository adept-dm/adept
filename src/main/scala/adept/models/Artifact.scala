package adept.models

import java.io.File

case class Artifact(hash: Hash, artifactType: String, configurations: Set[String], locations: Set[String])

object Artifact{
  def fromFile(file: File, artifactType: String, configurations: Set[String], locations: Set[String]) = {
    Artifact(Hash.calculate(file), artifactType, configurations, locations)
  }
}