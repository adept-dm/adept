package adept.models

import java.io.File

object Artifact{

  type ArtifactType = ArtifactType.Value
  object ArtifactType extends Enumeration {
    type ArtifactType = Value
    val jar, src, doc = Value
  }
  
  def typeFromFile(file: File) = {
    file.getName match {
      case n if n.endsWith("sources.jar") => ArtifactType.src
      case n if n.endsWith("javadoc.jar") => ArtifactType.doc
      case n if n.endsWith(".jar") => ArtifactType.jar
      case _ => throw new Exception("Odd artifact type")
    }
  }

  def fromFile(file: File, locations: Set[String]) = {
    Artifact(typeFromFile(file), Hash.calculate(file), locations)
  }
}

case class Artifact(artifactType: Artifact.ArtifactType, hash: Hash, locations: Set[String])

