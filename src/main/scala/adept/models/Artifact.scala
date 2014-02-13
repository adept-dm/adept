package adept.models

//Split models package into things that affects resolution and things  that does not (like Artifact, Hash, Ordering)
case class Artifact(hash: Hash, size: Long, locations: Set[String])
