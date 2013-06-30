package adept.core.models

class Evicted(val reason: String)

case class EvictedArtifact(artifact: Artifact, override val reason: String) extends Evicted(reason)

case class EvictedModule(module: Module, override val reason: String) extends Evicted(reason)

case class MissingDependency(dependency: Dependency, parent: Coordinates, override val reason: String) extends Evicted(reason)
