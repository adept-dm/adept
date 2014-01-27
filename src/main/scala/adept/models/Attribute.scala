package adept.models

case class Attribute(name: String, values: Set[String]) {
  def toConstraint: Constraint = Constraint(name, values)
}