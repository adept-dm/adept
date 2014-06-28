package adept.resolution.models

import adept.utils.OrderingHelpers
import com.fasterxml.jackson.core.{JsonParser, JsonGenerator}
import adept.services.{JsonService}
import adept.artifact.models.JsonSerializable

case class Attribute(name: String, values: Set[String]) extends JsonSerializable {
  def toConstraint: Constraint = Constraint(name, values)

  def writeJson(generator: JsonGenerator): Unit = {
    generator.writeStringField("name", name)
    JsonService.writeStringArrayField("values", values, generator)
  }
}

object Attribute {
  def fromJson(parser: JsonParser): Attribute = {
    JsonService.parseObject(parser, Map(
      ("name", _.getValueAsString),
      ("values", JsonService.parseStringSet(_))
    ),
      valueMap => Attribute(valueMap.get("name"), valueMap.getSet[String]("values"))
    )
  }

  implicit val ordering: Ordering[Attribute] = new Ordering[Attribute] {
    def compare(x: Attribute, y: Attribute): Int = {
      if (x.name < y.name)
        -1
      else if (x.name > y.name)
        1
      else {
        assert(x.name == y.name)
        OrderingHelpers.stringSetCompare(x.values, y.values)
      }
    }
  }
}
