package adept.resolution.models

import adept.utils.OrderingHelpers
import com.fasterxml.jackson.core.{JsonParser, JsonGenerator}
import adept.services.{JsonService}
import adept.artifact.models.JsonSerializable

case class Requirement(id: Id, constraints: Set[Constraint], exclusions: Set[Id]) extends JsonSerializable {
  override def toString = id + " " + constraints.map(c => c.name + "=" + c.values.mkString("(", ",", ")"))
    .mkString("[", ",", "]") + (if (exclusions.nonEmpty) exclusions.mkString("![", " & ", "]") else "")

  def constraint(name: String) = {
    val values = constraints.collect {
      case constraint if constraint.name == name => constraint.values
    }.flatten
    Constraint(name, values)
  }

  def writeJson(generator: JsonGenerator): Unit = {
    generator.writeStringField("id", id.value)
    JsonService.writeArrayField("constraints", constraints, generator)
    JsonService.writeStringArrayField("exclusions", exclusions.map(_.value), generator)
  }
}

object Requirement {
  def fromJson(parser: JsonParser): Requirement = {
    var id: Option[String] = None
    var constraints: Option[Set[Constraint]] = None
    var exclusions: Option[Set[String]] = None
    JsonService.parseObject(parser, (parser: JsonParser, fieldName: String) => {
      fieldName match {
        case "id" =>
          id = Some(parser.getValueAsString())
        case "constraints" =>
          constraints = Some(JsonService.parseSet(parser, () => {
            Constraint.fromJson(parser)
          }))
        case "exclusions" =>
          exclusions = Some(JsonService.parseStringSet(parser))
      }
    })
    Requirement(Id(id.get), constraints.get, exclusions.get.map(Id(_)))
  }

  implicit val ordering: Ordering[Requirement] = new Ordering[Requirement] {
    def compare(x: Requirement, y: Requirement): Int = {
      if (x.id.value < y.id.value)
        -1
      else if (x.id.value > y.id.value)
        1
      else {
        assert(x.id.value == y.id.value)
        if (x.constraints.size == y.constraints.size) {
          x.constraints.toSeq.sorted.zip(y.constraints.toSeq.sorted).foldLeft(0) {
            case (res, (cx, cy)) =>
              if (res == 0) {
                val constraintOrder = Constraint.ordering.compare(cx, cy)
                if (constraintOrder == 0)
                  OrderingHelpers.stringSetCompare(x.exclusions.map(_.value), y.exclusions.map(_.value))
                else constraintOrder
              }
              else res
          }
        } else {
          x.constraints.size - y.constraints.size
        }
      }
    }
  }
}
