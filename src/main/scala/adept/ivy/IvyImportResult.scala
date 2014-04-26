package adept.ivy

import adept.resolution.models._
import adept.artifact.models._
import adept.repository.models._
import adept.ext._
import org.apache.ivy.core.module.descriptor.ExcludeRule
import java.io.File
import play.api.libs.json.Json._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.repository.metadata.VariantMetadata
import adept.repository.metadata.ArtifactMetadata

//Contains everything needed to be able to import from Ivy to Adept
case class IvyImportResult(variant: Variant, artifacts: Set[Artifact], localFiles: Map[ArtifactHash, File], repository: RepositoryName, versionInfo: Set[(RepositoryName, Id, Version)], excludeRules: Map[(Id, Id), Set[(String, String)]], extendsIds: Set[Id], allConfigIds: Set[Id])

case class VersionInfo(name: RepositoryName, id: Id, version: Version) //TODO: use this one in IvyImportResult as well!
case class AdeptExclude(on: Id, requirement: Id)
case class IvyExclude(org: String, name: String)
case class AdeptExcludeMapping(adeptExclude: AdeptExclude, ivyExcludes: Set[IvyExclude])

//TODO: should be private for now!
case class IvyImportResultMetadata(id: Id, variant: VariantMetadata, artifacts: Set[Artifact], repository: RepositoryName, versionInfo: Set[VersionInfo], excludeRules: Set[AdeptExcludeMapping], extendsIds: Set[Id], allConfigIds: Set[Id])

case class IvyImportResultsMetadata(values: Set[IvyImportResultMetadata]) {
  val jsonString = Json.prettyPrint(Json.toJson(this))

  def toIvyImportResults = {
    values.map { metadata =>
      IvyImportResult(
        variant = metadata.variant.toVariant(metadata.id),
        artifacts = metadata.artifacts,
        localFiles = Map.empty,
        repository = metadata.repository,
        versionInfo = metadata.versionInfo.map { case VersionInfo(name, id, version) => (name, id, version) },
        excludeRules = metadata.excludeRules.map { case AdeptExcludeMapping( AdeptExclude(on, requirement), ivys) => ((on, requirement), ivys.map { case IvyExclude(org, name) => (org, name) } ) }.toMap,
        extendsIds = metadata.extendsIds,
        allConfigIds = metadata.allConfigIds)
    }
  }
}
object IvyImportResultsMetadata {
  private[adept] implicit val ivyImportResultFormatId: Format[Id] = Json.format[Id]
  private[adept] implicit val ivyImportResultFormatArtifactHash: Format[ArtifactHash] = Json.format[ArtifactHash]
  private[adept] implicit val ivyImportResultFormatArtifact: Format[Artifact] = Json.format[Artifact]
  private[adept] implicit val ivyImportResultFormatVersion: Format[Version] = Json.format[Version]
  private[adept] implicit val ivyImportResultFormatRepositoryName: Format[RepositoryName] = Json.format[RepositoryName]
  private[adept] implicit val ivyImportResultFormatVersionInfo: Format[VersionInfo] = Json.format[VersionInfo]
  private[adept] implicit val ivyImportResultFormatAdeptExclude: Format[AdeptExclude] = Json.format[AdeptExclude]
  private[adept] implicit val ivyImportResultFormatIvyExclude: Format[IvyExclude] = Json.format[IvyExclude]
  private[adept] implicit val ivyImportResultFormatAdeptExcludeMapping: Format[AdeptExcludeMapping] = Json.format[AdeptExcludeMapping]
  private[adept] implicit val ivyImportResultFormatMetadata: Format[IvyImportResultMetadata] = Json.format[IvyImportResultMetadata]

  private[adept] implicit val ivyImportResultFormat: Format[IvyImportResultsMetadata] = {
    Json.format[IvyImportResultsMetadata]
  }

  def fromResults(results: Set[IvyImportResult]) = {
    IvyImportResultsMetadata(results.map { result =>
      IvyImportResultMetadata(
        id = result.variant.id,
        variant = VariantMetadata.fromVariant(result.variant),
        artifacts = result.artifacts,
        repository = result.repository,
        versionInfo = result.versionInfo.map { case (name, id, version) => VersionInfo(name, id, version) },
        excludeRules = result.excludeRules.map { case ((on, requirement), ivys) => AdeptExcludeMapping(AdeptExclude(on, requirement), ivys.map { case (org, name) => IvyExclude(org, name) }) }.toSet,
        extendsIds = result.extendsIds,
        allConfigIds = result.allConfigIds)
    })
  }

  def read(json: JsValue) = {
    Json.fromJson[IvyImportResultsMetadata](json)
  }
}