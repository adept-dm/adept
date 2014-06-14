package adept.resolution.models

import adept.utils.OrderingHelpers
import com.fasterxml.jackson.core.{JsonParser, JsonGenerator}
import adept.services.JsonService

case class Attribute(name: String, values: Set[String]) {
  def toConstraint: Constraint = Constraint(name, values)
  
  def writeJson(generator: JsonGenerator) {
    JsonService.writeObject(generator, () => {
      generator.writeStringField("name", name)
      JsonService.writeStringArrayField("values", values, generator)
    })
  }
}

object Attribute {
  def fromJson(parser: JsonParser): Attribute = {
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

    Attribute(name.get, values.get)
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
