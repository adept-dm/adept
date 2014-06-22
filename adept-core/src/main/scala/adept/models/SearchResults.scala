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
