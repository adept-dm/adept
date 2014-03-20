package adept.resolution.models

import adept.utils.OrderingHelpers

case class Requirement(id: Id, constraints: Set[Constraint], exclusions: Set[Id]) {
  override def toString = id + " " + constraints.map(c => c.name + "=" + c.values.mkString("(", ",", ")")).mkString("[", ",", "]") + (if (exclusions.nonEmpty) exclusions.mkString("![", " & ", "]") else "")

  def constraint(name: String) = {
    val values = constraints.collect {
      case constraint if constraint.name == name => constraint.values
    }.flatten
    Constraint(name, values)
  }
}

object Requirement {
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
