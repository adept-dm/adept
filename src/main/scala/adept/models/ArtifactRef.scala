package adept.models

/**
 * An `ArtifactRef` is a reference (looks up using the `hash`) to an `Artifact`
 * 
 * It has an optional filename, which can be used to copy the actual artifact.
 */
case class ArtifactRef(hash: Hash, attributes: Set[Attribute], filename: Option[String]) {
  def attribute(name: String) = {
    val values = attributes.collect {
      case artifact if artifact.name == name => artifact.values
    }.flatten
    Attribute(name, values)
  }
  
  override def toString = {
    hash  + (if (filename.isDefined) ":" +filename.get else "") + "; " + attributes.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("[", ",", "]")
  }
}