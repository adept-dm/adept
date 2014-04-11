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

//Contains everything needed to be able to import from Ivy to Adept
case class IvyImportResult(variant: Variant, artifacts: Set[Artifact], localFiles: Map[ArtifactHash, File], repository: RepositoryName, versionInfo: Set[(RepositoryName, Id, Version)], excludeRules: Map[(Id, Id), Set[(String, String)]], extendsIds: Set[Id])

//case class IvyImportResults(ivyImportResults: Set[IvyImportResult]) {
//  val jsonString = {
//    Json.toJson(this)
//  }
//}
//
//object IvyImportResults {
//  private[adept] implicit val ivyImportResultFormat: Format[IvyImportResult] = {
//    Json.format[IvyImportResult]
//  }
//}