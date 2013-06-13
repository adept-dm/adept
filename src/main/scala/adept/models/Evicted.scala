package adept.models

class Evicted(val reason: String)

case class EvictedArtifact(artifact: Artifact, override val reason: String) extends Evicted(reason)

case class EvictedDependency(dependency: Dependency, module: Module, override val reason: String) extends Evicted(reason)
