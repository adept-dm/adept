package adept.models

case class Requirement(id: Id, constraints: Set[Constraint]) {
  override def toString = id + " " + constraints.map(c => c.name + "=" + c.values.mkString("(", ",", ")")).mkString("[", ",", "]")

  def constraint(name: String) = {
    val values = constraints.collect {
      case constraint if constraint.name == name => constraint.values
    }.flatten
    Constraint(name, values)
  }
}
