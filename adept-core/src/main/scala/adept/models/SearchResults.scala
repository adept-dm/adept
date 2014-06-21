package adept.models

import adept.resolution.models._
import adept.repository.models.{Commit, RepositoryName, RankId}


sealed class SearchResult(val variant: Variant, val rankId: RankId, val repository: RepositoryName,
  val isImport: Boolean)

case class ImportSearchResult(override val variant: Variant, override val rankId: RankId,
  override val repository: RepositoryName) extends SearchResult(variant, rankId, repository, isImport = true)

case class GitSearchResult(override val variant: Variant, override val rankId: RankId,
  override val repository: RepositoryName, commit: Commit, locations: Seq[String], isLocal: Boolean = false)
  extends SearchResult(variant, rankId, repository, isImport = false)

object GitSearchResult {
//  implicit val format: Format[GitSearchResult] = {
//    (
//      (__ \ "id").format[String] and
//      (__ \ "variant").format[VariantMetadata] and
//      (__ \ "rank").format[String] and
//      (__ \ "repository").format[String] and
//      (__ \ "commit").format[String] and
//      (__ \ "locations").format[Seq[String]])({
//        case (id, variant, rank, repository, commit, locations) =>
//         GitSearchResult(variant.toVariant(Id(id)), RankId(rank), RepositoryName(repository), Commit(commit), locations)
//      },
//        unlift({ gsr: GitSearchResult =>
//          val GitSearchResult(variant, rank, repository, commit, locations, offline) = gsr
//          Some(variant.id.value, VariantMetadata.fromVariant(variant), rank.value, repository.value, commit.value, locations)
//        }))
//  }
}
