package adept.repository.models.serialization

import adept.models._
import adept.repository.models._
import adept.repository.models.configuration._

private[models] object AdeptFormats {

  import play.api.libs.json._
  import play.api.libs.functional.syntax._

  private implicit val formatArtifactRef = {
    ///case class ArtifactRef(hash: Hash, attributes: Set[Attribute], filename: Option[String]) {
    (
      (__ \ "hash").format[String] and
      (__ \ "attributes").format[Map[String, Set[String]]] and
      (__ \ "filename").format[Option[String]])({
        case (hashString, attributes, filename) =>
          ArtifactRef(Hash(hashString),
            attributes.map { case (name, values) => Attribute(name, values) }.toSet,
            filename)
      }, unlift({ a: ArtifactRef =>
        val ArtifactRef(hash, attributes, filename) = a
        Some((hash.value,
          attributes.map(o => o.name -> o.values).toMap,
          filename))
      }))
  }

  //case class ConfiguredRequirement(id: Id, configurations: Set[ConfigurationId], constraints: Set[Constraint]) {
  private implicit val formatConfiguredRequirement = {
    (
      (__ \ "id").format[String] and
      (__ \ "configurations").format[Set[String]] and
      (__ \ "constraints").format[Map[String, Set[String]]])({
        case (idString, configurations, constraints) =>
          ConfiguredRequirement(Id(idString),
            configurations.map(ConfigurationId(_)).toSet,
            constraints.map { case (name, values) => Constraint(name, values) }.toSet)
      }, unlift({ c: ConfiguredRequirement =>
        val ConfiguredRequirement(id, configurations, constraints) = c
        Some((id.value,
          configurations.map(o => o.value).toSet,
          constraints.map(o => o.name -> o.values).toMap))
      }))
  }
  private implicit val formatConfiguration = {
    //case class Configuration(id: ConfigurationId, extendsConfigurations: Set[ConfigurationId], metadataInfo: Set[MetadataInfo], artifacts: Set[ArtifactRef], attributes: Set[Attribute], requirements: Set[ConfiguredRequirement])
    (
      (__ \ "id").format[String] and
      (__ \ "extends-configurations").format[Set[String]] and
      (__ \ "info").format[Map[String, Set[String]]] and
      (__ \ "artifacts").format[Set[ArtifactRef]] and
      (__ \ "attributes").format[Map[String, Set[String]]] and
      (__ \ "requirements").format[Set[ConfiguredRequirement]])({
        case (idString, extendsConfigurations, metadataInfo, artifacts, attributes, requirements) =>
          Configuration(ConfigurationId(idString),
            extendsConfigurations.map(ConfigurationId(_)),
            metadataInfo.map { case (name, values) => MetadataInfo(name, values) }.toSet,
            artifacts,
            attributes.map { case (name, values) => Attribute(name, values) }.toSet,
            requirements)
      }, unlift({ c: Configuration =>
        val Configuration(id, extendsConfigurations, metadataInfo, artifacts, attributes, requirements) = c
        Some((id.value,
          extendsConfigurations.map(_.value),
          metadataInfo.map(o => o.name -> o.values).toMap,
          artifacts,
          attributes.map(o => o.name -> o.values).toMap,
          requirements))
      }))
  }

  implicit val formatVariantMetadata: Format[VariantMetadata] = {
    (
      (__ \ "id").format[String] and
      (__ \ "info").format[Map[String, Set[String]]] and
      (__ \ "attributes").format[Map[String, Set[String]]] and
      (__ \ "configurations").format[Set[Configuration]])({
        case (idString, metadataInfo, attributes, configurations) => VariantMetadata(Id(idString),
          metadataInfo.map { case (name, values) => MetadataInfo(name, values) }.toSet,
          attributes.map { case (name, values) => Attribute(name, values) }.toSet,
          configurations)
      }, unlift({
        case VariantMetadata(id, metadataInfo, attributes, configurations) =>
          Some((id.value,
            metadataInfo.map(o => o.name -> o.values).toMap,
            attributes.map(o => o.name -> o.values).toMap, configurations))
      }))
  }
  //artifacts
  implicit val formatArtifactMetadata = {
    //case class ArtifactMetadata(hash: Hash, size: Long, locations: Set[String]) {
    (
      (__ \ "hash").format[String] and
      (__ \ "size").format[Long] and
      (__ \ "locations").format[Set[String]])({
        case (hashString, size, locations) =>
          ArtifactMetadata(Hash(hashString), size, locations)
      }, unlift({ a: ArtifactMetadata =>
        val ArtifactMetadata(hash, size, locations) = a
        Some((hash.value,
          size,
          locations))
      }))
  }

  //repository:
  private implicit val formatRepositoryInfo = {
    //case class RepositoryInfo(id: Id, repository: String, commit: Commit)
    (
      (__ \ "id").format[String] and
      (__ \ "repository").format[String] and
      (__ \ "commit").format[String])({
        case (idString, repository, commit) =>
          RepositoryInfo(Id(idString), repository, Commit(commit))
      }, unlift({ r: RepositoryInfo =>
        val RepositoryInfo(id, repository, commit) = r
        Some((id.value,
          repository,
          commit.value))
      }))
  }

  private implicit val formatRepositoryConfiguration = {
    //case class RepositoryConfiguration(id: ConfigurationId, repositories: Seq[RepositoryInfo])
    (
      (__ \ "id").format[String] and
      (__ \ "repositories").format[Seq[RepositoryInfo]])({
        case (idString, repositories) =>
          RepositoryConfiguration(ConfigurationId(idString), repositories)
      }, unlift({ r: RepositoryConfiguration =>
        val RepositoryConfiguration(id, repositories) = r
        Some((id.value,
          repositories))
      }))
  }
  implicit val formatRepositoryMetadata = {
    //case class RepositoryMetadata(id: Id, variants: Set[Hash], configurations: Seq[RepositoryConfiguration]) {
    (
      (__ \ "id").format[String] and
      (__ \ "variants").format[Set[String]] and
      (__ \ "configurations").format[Seq[RepositoryConfiguration]])({
        case (idString, variants, configurations) =>
          RepositoryMetadata(Id(idString), variants.map(Hash(_)), configurations)
      }, unlift({ r: RepositoryMetadata =>
        val RepositoryMetadata(id, variants, configurations) = r
        Some((id.value,
          variants.map(_.value),
          configurations))
      }))
  }

  //lockfile:
  private implicit val requirementEntryFormat = {
    //case class LockFileRequirement(id: Id, configuration: ConfigurationId, constraints: Seq[Constraint], repositoryName: String, repositoryCommit: Commit) {

    (
      (__ \ "id").format[String] and
      (__ \ "configuration").format[String] and
      (__ \ "constraints").format[Map[String, Set[String]]] and
      (__ \ "repository").format[String] and
      (__ \ "commit").format[String])({
        case (idString, configurationString, constraints, repositoryName, commitString) =>
          LockFileRequirement(
            Id(idString),
            ConfigurationId(configurationString),
            constraints.map { case (name, values) => Constraint(name, values) }.toSeq,
            repositoryName,
            Commit(commitString))
      }, unlift({ r: LockFileRequirement =>
        val LockFileRequirement(id, configuration, constraints, repositoryName, commit) = r
        Some((id.value,
          configuration.value,
          constraints.map(o => o.name -> o.values).toMap,
          repositoryName,
          commit.value))
      }))
  }
  private implicit val artifactEntryFormat = {
    //case class LockFileArtifact(hash: Hash, size: Long, locations: Set[String], filename: Option[String])
    (
      (__ \ "hash").format[String] and
      (__ \ "size").format[Long] and
      (__ \ "locations").format[Set[String]] and
      (__ \ "filename").format[Option[String]])({
        case (hashString, size, locations, filename) =>
          LockFileArtifact(Hash(hashString), size, locations, filename)
      }, unlift({ a: LockFileArtifact =>
        val LockFileArtifact(hash, size, locations, filename) = a
        Some((hash.value,
          size,
          locations,
          filename))
      }))
  }
  
  implicit val lockFileFormat = {
    //case class LockFile(hash: Hash, requirements: Seq[LockFileRequirement], artifacts: Seq[LockFileArtifact]) {
        (
      (__ \ "hash").format[String] and
      (__ \ "requirements").format[Seq[LockFileRequirement]] and
      (__ \ "artifacts").format[Seq[LockFileArtifact]])({
        case (hashString, requirements, artifacts) =>
          LockFile(Hash(hashString), requirements, artifacts)
      }, unlift({ a: LockFile =>
        val LockFile(hash, requirements, artifacts) = a
        Some((hash.value,
          requirements,
          artifacts))
      }))
  }
} 
