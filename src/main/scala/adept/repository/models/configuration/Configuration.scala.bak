package adept.repository.models.configuration

import adept.models._
import adept.repository.models.MetadataInfo

object Configuration {
  val ConfigurationHashAttributeName = "configuration-hash"
  val ConfigurationDescriptionAttributeName = "configuration-description"
}

case class Configuration(id: ConfigurationId, extendsConfigurations: Set[ConfigurationId], metadataInfo: Set[MetadataInfo], artifacts: Set[ArtifactRef], attributes: Set[Attribute], requirements: Set[ConfiguredRequirement])
