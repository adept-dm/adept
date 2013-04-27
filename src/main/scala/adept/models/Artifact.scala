package adept.models

import java.io.File

object Artifact{

  type ArtifactType = ArtifactType.Value
  object ArtifactType extends Enumeration {
    type ArtifactType = Value
    val jar, src, doc = Value
  }
  

  def fromFile(file: File, locations: Set[String]) = {
    val suffix = file.getName.substring(file.getName.lastIndexOf('.'))
    Artifact(ArtifactType.withName(suffix), Hash.calculate(file), locations)
  }
}

case class Artifact(artifactType: Artifact.ArtifactType, hash: Hash, locations: Set[String])

