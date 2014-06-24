package adept.resolution.models

import com.fasterxml.jackson.core.JsonParser
import adept.services.JsonService

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
case class Variant(id: Id, attributes: Set[Attribute] = Set.empty, artifacts: Set[ArtifactRef] = Set.empty,
                   requirements: Set[Requirement] = Set.empty) {
  
  def attribute(name: String) = {
    val values = attributes.collect {
      case artifact if artifact.name == name => artifact.values
    }.flatten
    Attribute(name, values)
  }

  override lazy val toString = {
    id + " " + attributes.toSeq.sorted.map(a => a.name + "=" + a.values.mkString("(", ",", ")")).mkString("[", ",", "]")
  }

  lazy val fullString = {
    toString +
      (if (requirements.nonEmpty) requirements.toSeq.sorted.mkString("{", ",", "}") else "") +
      (if (artifacts.nonEmpty) artifacts.toSeq.sorted.mkString("|", ",", "|") else "")
  }
}

object Variant {
  def fromJson(parser: JsonParser): Variant = {
    var id: Option[Id] = null
    var attributes: Set[Attribute] = Set[Attribute]()
    var artifacts = Set[ArtifactRef]()
    var requirements = Set[Requirement]()

    JsonService.parseObject(parser, (parser, fieldName) => {
      fieldName match {
        case "id" => id = Some(Id(parser.getValueAsString()))
        case "attributes" => attributes = JsonService.parseSet(parser, () =>
          Attribute.fromJson(parser)
        )
        case "artifacts" =>
          artifacts = JsonService.parseSet(parser, () => ArtifactRef.fromJson(parser))
        case "requirements" => requirements = JsonService.parseSet(parser, () => Requirement.fromJson(parser))
      }
    })

    Variant(id.get, attributes, artifacts, requirements)
  }
}
