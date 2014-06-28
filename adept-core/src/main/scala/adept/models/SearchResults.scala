package adept.models

import adept.resolution.models._
import adept.repository.models.{Commit, RepositoryName, RankId}
import com.fasterxml.jackson.core.JsonParser
import adept.services.JsonService


sealed class SearchResult(val variant: Variant, val rankId: RankId, val repository: RepositoryName,
  val isImport: Boolean)

case class ImportSearchResult(override val variant: Variant, override val rankId: RankId,
  override val repository: RepositoryName) extends SearchResult(variant, rankId, repository, isImport = true)

case class GitSearchResult(override val variant: Variant, override val rankId: RankId,
  override val repository: RepositoryName, commit: Commit, locations: Seq[String], isLocal: Boolean = false)
  extends SearchResult(variant, rankId, repository, isImport = false)

object GitSearchResult {
  def fromJson(parser: JsonParser): GitSearchResult = {
    var variant: Option[Variant] = None
    var rankId: Option[RankId] = None
    var repository: Option[RepositoryName] = None
    var commit: Option[Commit] = None
    var locations: Seq[String] = Seq[String]()
    var isLocal: Boolean = false
    JsonService.parseObject(parser, (parser, fieldName) => {
      fieldName match {
        case "variant" => variant = Some(Variant.fromJson(parser))
        case "rankId" => rankId = Some(RankId(parser.getValueAsString))
        case "repository" => repository = Some(RepositoryName(parser.getValueAsString))
        case "commit" => commit = Some(Commit(parser.getValueAsString))
        case "locations" => locations = JsonService.parseStringSeq(parser)
        case "isLocal" => isLocal = parser.getValueAsBoolean
      }
    })

    GitSearchResult(variant.get, rankId.get, repository.get, commit.get, locations, isLocal)
  }
}
