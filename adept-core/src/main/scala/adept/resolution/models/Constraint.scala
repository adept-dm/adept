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
    var name: Option[String] = null
    var values: Option[Set[String]] = null
    JsonService.parseObject(parser, (parser: JsonParser, fieldName: String) => {
      fieldName match {
        case "name" =>
          name = Some(parser.getValueAsString())
        case "values" =>
          values = Some(JsonService.parseStringSet(parser))
      }
    })
    Constraint(name.get, values.get)
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
