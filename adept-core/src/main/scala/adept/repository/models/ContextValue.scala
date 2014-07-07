package adept.repository.models

import adept.resolution.models._
import com.fasterxml.jackson.core.{JsonParser, JsonGenerator}
import adept.services.JsonService
import adept.artifact.models.JsonSerializable

/** The context value for each Id: answers the who we found (the variant hash) and where we found it (commit and repository) */
case class ContextValue(id: Id, repository: RepositoryName, commit: Option[Commit], variant: VariantHash)
  extends JsonSerializable {
  def writeJson(generator: JsonGenerator): Unit = {
    generator.writeStringField("id", id.value)
    generator.writeStringField("repository", repository.value)
    JsonService.writeStringField("commit", commit.map(_.value), generator)
    generator.writeStringField("variant", variant.value)
  }
}

//TODO: rename variant to hash

object ContextValue {
  def fromJson(parser: JsonParser): ContextValue = {
    JsonService.parseObject(parser, Map(
      ("id", _ getValueAsString),
      ("repository", _.getValueAsString),
      ("commit", parser => Commit(parser.getValueAsString)),
      ("variant", _.getValueAsString)
    ), valueMap => ContextValue(Id(valueMap.getString("id")),
      RepositoryName(valueMap.getString("repository")), valueMap.getOption[Commit]("commit"),
      VariantHash(valueMap.get("variant"))))
  }

  implicit val ordering: Ordering[ContextValue] = new Ordering[ContextValue] {
    def compare(x: ContextValue, y: ContextValue): Int = {
      if (x.repository.value < y.repository.value)
        -1
      else if (x.repository.value > y.repository.value)
        1
      else {
        if (x.id.value < y.id.value)
          -1
        else if (x.id.value > y.id.value)
          1
        else {
          if (x.commit.isDefined && y.commit.isDefined) {
            val xcommit = x.commit.get
            val ycommit = y.commit.get
            if (xcommit.value < ycommit.value)
              -1
            else if (xcommit.value > ycommit.value)
              1
            else {
              if (x.variant.value < y.variant.value)
                -1
              else if (x.variant.value > y.variant.value)
                1
              else {
                0
              }
            }
          } else {
            if (x.commit.isDefined && !y.commit.isDefined) {
              1
            } else {
              -1
            }
          }
        }
      }
    }
  }
}
