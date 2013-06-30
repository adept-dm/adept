package adept.core.models

case class DependencyExclusionRule(org: String, name: String, parent: Coordinates) {
  def matches(dependency: Dependency): Boolean = {
    (org, name) match {
      case ("*", "*") => true
      case ("*", _) if dependency.coordinates.name == name => true 
      case (_, _) if dependency.coordinates.org == org && dependency.coordinates.name == name => true 
      case (_, "*") if dependency.coordinates.org == org => true 
      case (_, _) => false
    }
  }
}
