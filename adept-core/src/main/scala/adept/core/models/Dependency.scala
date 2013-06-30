package adept.core.models

case class Dependency(coordinates: Coordinates, hash: Hash, configuration: String, isTransitive: Boolean = true, exclusionRules: Set[DependencyExclusionRule] = Set.empty) 