package adept.core.models

case class Dependency(id: Id, constraints: Set[Constraint]) {
  override def toString = id + " " + constraints.map(c => c.name + "=" + c.values.mkString("(", ",", ")")).mkString("[", ",", "]")
}
