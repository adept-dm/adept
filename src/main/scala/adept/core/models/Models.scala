package adept.core.models

case class Attribute(name: String, values: Set[String]) {
  def toConstraint: Constraint = Constraint(name, values)
}

case class Constraint(name: String, values: Set[String]) {
  def toAttribute: Attribute = Attribute(name, values)
}

case class Dependency(id: String, constraints: Set[Constraint])

case class Artifact(hash: String, attributes: Set[Attribute])

case class Variant(moduleId: String, artifacts: Set[Artifact], attributes: Set[Attribute], dependencies: Set[Dependency]) {
  def attribute(name: String) = {
    val values = attributes.collect {
      case artifact if artifact.name == name => artifact.values
    }.flatten
    Attribute(name, values)
  }

  override def toString = {
    moduleId + " " + attributes.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("[", ",", "]")
  }

}