package adept.core.models

import adept.configuration.ConfigurationId

/**
 * Use VariantBuilder to create variants
 */
case class Variant private[adept] (id: Id, artifacts: Set[ArtifactRef], attributes: Set[Attribute], dependencies: Set[Dependency], info: Set[(String, String)] = Set.empty) { //TODO: move artifacts <-> attributes (attributes are more core)
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
      (if (dependencies.nonEmpty) dependencies.mkString("{", ",", "}") else "") +
      (if (artifacts.nonEmpty) artifacts.mkString("|", ",", "|") else "")
  }
}