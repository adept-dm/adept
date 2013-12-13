package adept.core.models

case class Variant(id: Id, artifacts: Set[ArtifactRef], attributes: Set[Attribute], dependencies: Set[Dependency]) {
  def attribute(name: String) = {
    val values = attributes.collect {
      case artifact if artifact.name == name => artifact.values
    }.flatten
    Attribute(name, values)
  }

  override def toString = {
    id + " " + attributes.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("[", ",", "]")
  }

  def fullString = {
    toString + 
      (if (dependencies.nonEmpty) dependencies.mkString("{", ",", "}") else "") + 
      (if (artifacts.nonEmpty) artifacts.mkString("|", ",", "|") else "") 
  }
}