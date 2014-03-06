package adept.repository.models.serialization

import adept.models._
import adept.repository.models._
import adept.repository.models.configuration._

private[models] object AdeptFormats {

  import play.api.libs.json._

  private implicit val formatMetdataInfo = Json.format[MetadataInfo]
  private implicit val formatAttribute = Json.format[Attribute]
  private implicit val formatConfigurationId = Json.format[ConfigurationId]
  private implicit val formatHash = Json.format[Hash]
  private implicit val formatArtifactRef = Json.format[ArtifactRef]
  private implicit val formatCommit = Json.format[Commit]
  private implicit val formatConstraint = Json.format[Constraint]
  private implicit val formatId = Json.format[Id]
  private implicit val formatConfiguredRequirement = Json.format[ConfiguredRequirement]
  private implicit val formatConfiguration = Json.format[Configuration]
  implicit val formatConfiguredVariantsMetadata = Json.format[VariantMetadata]
  
  //artifacts
  implicit val formatArtifactMetadata = Json.format[ArtifactMetadata]
  
  //repository:
  private implicit val formatRepositoryInfo = Json.format[RepositoryInfo]
  private implicit val formatRepositoryConfiguration = Json.format[RepositoryConfiguration]
  implicit val formatRepositoryMetadata = Json.format[RepositoryMetadata]

  //lockfile:
  private implicit val requirementEntryFormat = Json.format[LockFileRequirement]
  private implicit val artifactEntryFormat = Json.format[LockFileArtifact]
  implicit val lockFileFormat = Json.format[LockFile]
} 
