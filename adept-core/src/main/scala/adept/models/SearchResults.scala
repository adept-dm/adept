package adept.models

import adept.artifact.models.JsonSerializable
import adept.repository.models.{Commit, RankId, RepositoryName}
import adept.resolution.models._
import adept.services.JsonService
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}

abstract sealed class SearchResult(val variant: Variant, val rankId: RankId, val repository: RepositoryName,
                                   val isImport: Boolean) extends JsonSerializable

case class ImportSearchResult(override val variant: Variant, override val rankId: RankId,
                              override val repository: RepositoryName) extends SearchResult(
  variant, rankId, repository,
  isImport = true) {
  override def writeJson(generator: JsonGenerator): Unit = {
    JsonService.writeObjectField("variant", variant, generator)
    generator.writeStringField("rankId", rankId.value)
    generator.writeStringField("repository", repository.value)
  }
}

case class GitSearchResult(override val variant: Variant, override val rankId: RankId,
                           override val repository: RepositoryName, commit: Commit, locations: Seq[String],
                           isLocal: Boolean = false)
  extends SearchResult(variant, rankId, repository, isImport = false) {
  override def writeJson(generator: JsonGenerator): Unit = {
    JsonService.writeObjectField("variant", variant, generator)
    generator.writeStringField("rankId", rankId.value)
    generator.writeStringField("repository", repository.value)
    generator.writeStringField("commit", commit.value)
    JsonService.writeStringArrayField("locations", locations, generator)
    generator.writeBooleanField("isLocal", isLocal)
  }
}

object GitSearchResult {
  def fromJson(parser: JsonParser): GitSearchResult = {
    JsonService.parseObject(parser, Map(
      ("variant", Variant.fromJson),
      ("rankId", _.getValueAsString),
      ("repository", _.getValueAsString),
      ("commit", _.getValueAsString),
      ("locations", JsonService.parseStringSeq),
      ("isLocal", _.getValueAsBoolean)
    ), { valueMap =>
      GitSearchResult(valueMap.get[Variant]("variant"),
        RankId(valueMap.getString("rankId")),
        RepositoryName(valueMap.getString("repository")), Commit(valueMap.getString("commit")),
        valueMap.getStringSeq("locations"), valueMap.getOrElse[Boolean]("isLocal", false))
    })
  }
}
