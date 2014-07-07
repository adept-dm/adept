package adept.resolution.models

import adept.artifact.models._
import collection.JavaConverters._
import adept.utils.OrderingHelpers
import com.fasterxml.jackson.core.{JsonParser, JsonGenerator}
import adept.services.JsonService

/**
 * An `ArtifactRef` is a reference (looks up using the `hash`) to an `Artifact`
 *
 * It has an optional filename, which can be used to copy the actual artifact.
 */
case class ArtifactRef(hash: ArtifactHash, attributes: Set[ArtifactAttribute], filename: Option[String])
  extends JsonSerializable{
  def attribute(name: String) = {
    val values = attributes.collect {
      case artifact if artifact.name == name => artifact.values.asScala
    }.flatten
    new ArtifactAttribute(name, values.asJava)
  }

  override def toString = {
    hash + (if (filename.isDefined) ":" + filename.get else "") + "; " +
      attributes.map(a => a.name + "=" + a.values.asScala.mkString("(", ",", ")")).mkString("[", ",", "]")
  }

  def writeJson(generator: JsonGenerator): Unit = {
      generator.writeStringField("hash", hash.value)
      JsonService.writeArrayField("attributes", attributes, generator)
      filename.map { generator.writeStringField("filename", _) }
  }
}

object ArtifactRef {
  def fromJson(parser: JsonParser): ArtifactRef = {
    JsonService.parseObject(parser, Map(
      ("hash", _.getValueAsString),
      ("attributes", JsonService.parseSet(_, ArtifactAttribute.fromJson)),
      ("filename", _.getValueAsString)
    ), valueMap => ArtifactRef(new ArtifactHash(valueMap.getString("hash")),
      valueMap.getSet[ArtifactAttribute]("attributes"), valueMap.getOption[String]("filename")))
  }

  implicit val orderingArtifactAttribute: Ordering[ArtifactAttribute] = new Ordering[ArtifactAttribute] {
    def compare(x: ArtifactAttribute, y: ArtifactAttribute): Int = {
      if (x.name < y.name)
        -1
      else if (x.name > y.name)
        1
      else {
        assert(x.name == y.name)
        OrderingHelpers.stringSetCompare(Set() ++ x.values.asScala, Set() ++ y.values.asScala)
      }
    }
  }
  implicit val orderingArtifactRef: Ordering[ArtifactRef] = new Ordering[ArtifactRef] {
    def compare(x: ArtifactRef, y: ArtifactRef): Int = {
      if (x.hash.value < y.hash.value)
        -1
      else if (x.hash.value > y.hash.value)
        1
      else {
        assert(x.hash.value == y.hash.value)
        if (x.attributes.size == y.attributes.size) {
          val res = x.attributes.toSeq.sorted.zip(y.attributes.toSeq.sorted).foldLeft(0) {
            case (res, (a, b)) =>
              if (res == 0) orderingArtifactAttribute.compare(a, b)
              else res
          }
          if (res == 0) {
            val s = x.filename.size - y.filename.size
            if (s == 0) {
              scala.math.Ordering.String.compare(x.filename.toString, y.filename.toString)
            } else s
          } else res
        } else x.attributes.size - y.attributes.size
      }
    }
  }
}
