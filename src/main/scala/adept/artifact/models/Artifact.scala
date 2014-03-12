package adept.artifact.models

case class Artifact(hash: ArtifactHash, size: Long, locations: Set[String])
