package adept.core.models

case class Artifact(hash: Hash, size: Long, locations: Set[String])
