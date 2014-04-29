package adept.resolution.models

import adept.utils.OrderingHelpers

case class Constraint(name: String, values: Set[String]) {
  def toAttribute: Attribute = Attribute(name, values)
}

object Constraint {
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