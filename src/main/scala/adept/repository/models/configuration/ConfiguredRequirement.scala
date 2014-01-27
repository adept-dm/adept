package adept.repository.models.configuration

import adept.repository.models.RepositoryMetadata
import adept.models._

/**
 * Merges a configuration into a requirement (changes Id)
 */
//TODO: configurations should be ConfigurationId, but I can't get it to deserialize correctly
case class ConfiguredRequirement(id: Id, configurations: Set[ConfigurationId], commit: RepositoryMetadata, constraints: Set[Constraint])
