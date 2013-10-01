package adept.core.models

case class Attribute(name: String, values: Set[String])

case class Constraint(name: String, values: Set[String])

case class Dependency(id: String, constraints: Set[Constraint])

case class Artifact(hash: String, constraints: Set[Constraint])

case class Variant(moduleId: String, artifacts: Set[Artifact], attributes: Set[Attribute], dependencies: Seq[Dependency]) {
  override def toString = {
    moduleId + " " + attributes.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("[",",", "]")
  }
  
}