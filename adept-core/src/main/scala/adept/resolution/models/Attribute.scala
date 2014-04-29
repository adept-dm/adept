package adept.resolution.models

import adept.utils.OrderingHelpers

case class Attribute(name: String, values: Set[String]) {
  def toConstraint: Constraint = Constraint(name, values)
}

object Attribute {
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