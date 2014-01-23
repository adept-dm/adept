package adept.core.models

//Rename Dependency to CoreRequirement and ConfiguredDependency to Requirement
case class Dependency(id: Id, constraints: Set[Constraint]) { //TODO: replace id: id: DependencyId/RequirementId
  override def toString = id + " " + constraints.map(c => c.name + "=" + c.values.mkString("(", ",", ")")).mkString("[", ",", "]")

  def constraint(name: String) = {
    val values = constraints.collect {
      case constraint if constraint.name == name => constraint.values
    }.flatten
    Constraint(name, values)
  }
}
