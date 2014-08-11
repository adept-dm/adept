package adept.resolution.models

import adept.artifact.models.JsonSerializable
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
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
case class Variant(id: Id, attributes: Set[Attribute] = Set.empty, artifacts: Set[ArtifactRef] =
Set.empty, requirements: Set[Requirement] = Set.empty) extends JsonSerializable {

  def attribute(name: String) = {
    val values = attributes.collect {
      case artifact if artifact.name == name => artifact.values
    }.flatten
    Attribute(name, values)
  }

  override lazy val toString = {
    id + " " + attributes.toSeq.sorted.map(a => a.name + "=" + a.values.mkString("(", ",", ")"))
      .mkString("[", ",", "]")
  }

  lazy val fullString = {
    toString +
      (if (requirements.nonEmpty) requirements.toSeq.sorted.mkString("{", ",", "}") else "") +
      (if (artifacts.nonEmpty) artifacts.toSeq.sorted.mkString("|", ",", "|") else "")
  }

  override def writeJson(generator: JsonGenerator): Unit = {
    generator.writeStringField("id", id.value)
    JsonService.writeArrayField("attributes", attributes, generator)
    JsonService.writeArrayField("artifacts", artifacts, generator)
    JsonService.writeArrayField("requirements", requirements, generator)
  }
}

object Variant {
  def fromJson(parser: JsonParser): Variant = {
    JsonService.parseObject(parser, Map(
      ("id", _.getValueAsString),
      ("attributes", JsonService.parseSet(_, Attribute.fromJson)),
      ("artifacts", JsonService.parseSet(_, ArtifactRef.fromJson)),
      ("requirements", JsonService.parseSet(_, Requirement.fromJson))
    ), valueMap => Variant(Id(valueMap.getString("id")), valueMap.getSet[Attribute]("attributes"),
      valueMap.getSet[ArtifactRef]("artifacts"), valueMap.getSet[Requirement]("requirements")))
  }
}
