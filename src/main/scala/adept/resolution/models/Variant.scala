package adept.resolution.models

import adept.artifact.models.ArtifactRef

/**
 * A variant is the "atom" of Adept, in the sense that it
 * is not the smallest unit, but sort of...
 * 
 * It is the equivalent of 'version' in many dependency managers, 
 * but since Adept is awesome (yeah, that is right) and multi-dimensional 
 * (meaning it can resolve on multiple attributes) you could risk having 
 * many variants with the same "version" (e.g. 2 variants with "version": 1.0.0, 
 * but with different requirements). Therefore, 'version' would be a poor choice 
 * for a name while variant fits the bill perfectly.
 */
case class Variant(id: Id, attributes: Set[Attribute] = Set.empty, artifacts: Set[ArtifactRef] = Set.empty, requirements: Set[Requirement] = Set.empty, exclusions: Set[Id] = Set.empty) {
  
  def attribute(name: String) = {
    val values = attributes.collect {
      case artifact if artifact.name == name => artifact.values
    }.flatten
    Attribute(name, values)
  }

  override lazy val toString = {
    id + " " + attributes.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("[", ",", "]")
  }

  lazy val fullString = {
    toString +
      (if (requirements.nonEmpty) requirements.mkString("{", ",", "}") else "") +
      (if (artifacts.nonEmpty) artifacts.mkString("|", ",", "|") else "")
  }
}