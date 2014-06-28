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
    var id: Option[String] = None
    var repository: Option[String] = None
    var commit: Option[String] = None
    var variant: Option[String] = None
    JsonService.parseObject(parser, (parser, fieldName) => {
      fieldName match {
        case "id" =>
          id = Some(parser.getValueAsString())
        case "repository" =>
          repository = Some(parser.getValueAsString())
        case "commit" =>
          commit = Some(parser.getValueAsString())
        case "variant" =>
          variant = Some(parser.getValueAsString())
      }
    })

    ContextValue(Id(id.get), RepositoryName(repository.get), Some(Commit(commit.get)),
      VariantHash(variant.get))
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
