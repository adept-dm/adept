package adept.repository.models.configuration

import adept.models._
import adept.repository.models.MetadataInfo

case class Configuration(id: ConfigurationId, extendsConfigurations: Set[ConfigurationId], metadata: Set[MetadataInfo], artifacts: Set[ArtifactRef], attributes: Set[Attribute], requirements: Set[ConfiguredRequirement])
