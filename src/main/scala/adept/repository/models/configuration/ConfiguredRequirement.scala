package adept.repository.models.configuration

import adept.repository.models.RepositoryMetadata
import adept.models._
import adept.repository.AdeptGitRepository

/**
 * Merges a configuration into a requirement (changes Id)
 */
//TODO: configurations should be ConfigurationId, but I can't get it to deserialize correctly
case class ConfiguredRequirement(id: Id, configurations: Set[ConfigurationId], constraints: Set[Constraint]) {
  def asRequirements(repositoryName: String): Set[Requirement] = configurations.map { configuration =>
    Requirement(Id(repositoryName + Id.Sep + ConfigurationId.join(id, configuration).value), constraints)
  }
}