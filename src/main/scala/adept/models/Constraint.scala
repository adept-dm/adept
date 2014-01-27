package adept.models

case class Constraint(name: String, values: Set[String]) {
  def toAttribute: Attribute = Attribute(name, values)
}