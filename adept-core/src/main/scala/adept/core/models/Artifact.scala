package adept.core.models

import java.io.File

case class Artifact(hash: Hash, artifactType: String, configurations: Set[String] = Set(Artifact.DefaultConf), locations: Set[String])

object Artifact{
  private[models] val DefaultConf = "*"
  
  def fromFile(file: File, artifactType: String, configurations: Set[String], locations: Set[String]) = {
    Artifact(Hash.calculate(file), artifactType, configurations, locations)
  }
}
