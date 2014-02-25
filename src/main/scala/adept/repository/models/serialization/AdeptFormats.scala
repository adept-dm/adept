package adept.repository.models.serialization

import adept.models._
import adept.repository.models._
import adept.repository.models.configuration._

private[models] object AdeptFormats {
  
  import play.api.libs.json._

  implicit val formatMetdataInfo = Json.format[MetadataInfo]
  implicit val formatAttribute = Json.format[Attribute]
  implicit val formatConfigurationId = Json.format[ConfigurationId]
  implicit val formatHash = Json.format[Hash]
  implicit val formatArtifactRef = Json.format[ArtifactRef]
  implicit val formatCommit = Json.format[Commit]
  implicit val formatConstraint = Json.format[Constraint]
  implicit val formatId = Json.format[Id]
  implicit val formatConfiguredRequirement = Json.format[ConfiguredRequirement]
  implicit val formatConfiguration = Json.format[Configuration]
  implicit val formatConfiguredVariantsMetadata = Json.format[ConfiguredVariantsMetadata]
  implicit val formatArtifactMetadata = Json.format[ArtifactMetadata]
  implicit val formatRepositoryInfo = Json.format[RepositoryInfo]
  implicit val formatRepositoryConfiguration = Json.format[RepositoryConfiguration]
  implicit val formatRepositoryMetadata = Json.format[RepositoryMetadata]
} 
