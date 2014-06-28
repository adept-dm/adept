package adept.resolution.models

import adept.utils.OrderingHelpers
import com.fasterxml.jackson.core.{JsonParser, JsonGenerator}
import adept.services.JsonService
import adept.artifact.models.JsonSerializable

case class Constraint(name: String, values: Set[String]) extends JsonSerializable {
  def toAttribute: Attribute = Attribute(name, values)

  def writeJson(generator: JsonGenerator): Unit = {
    generator.writeStringField("name", name)
    JsonService.writeStringArrayField("values", values, generator)
  }
}

object Constraint {
  def fromJson(parser: JsonParser): Constraint = {
    JsonService.parseObject(parser, Map(
      ("name", _.getValueAsString),
      ("values", JsonService.parseStringSet)
    ), valueMap => Constraint(valueMap.getString("name"), valueMap.getStringSet("values")))
  }

  implicit val ordering: Ordering[Constraint] = new Ordering[Constraint] {
    def compare(x: Constraint, y: Constraint): Int = {
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
