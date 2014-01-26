package adept.serialization

import adept.core.models._
import org.json4s._
import org.json4s.native.Serialization
import adept.configuration.ConfigurationId
import adept.repository.Commit

//TODO: merge Attribute/Constraint and Hash/Value
//TODO: create adept.serializer tests!!!
object Formats {

  implicit val formats = DefaultFormats +
    new ConfigurationIdSerializer + new AttributeSerializer + new ConstraintSerializer +
    new HashSerializer + new IdSerializer + new CommitSerializer +
     new MetadataSerializer

}

class AttributeSerializer extends Serializer[Attribute] {
  private val ThisClass = classOf[Attribute]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Attribute] = {
    case (TypeInfo(ThisClass, _), json) => json match {
      case v @ JObject(JField(name, JArray(rawValues)) :: _) =>
        val values = rawValues.map {
          case JString(v) => v
          case x => throw new MappingException("Can't convert " + x + " to String in " + v)
        }.toSet
        Attribute(name, values)
      case x => throw new MappingException("Can't convert " + x + " to " + ThisClass.getName)
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case v: Attribute =>
      import JsonDSL._
      (v.name -> v.values.toList.sorted)
  }
}

class ConstraintSerializer extends Serializer[Constraint] {
  private val ThisClass = classOf[Constraint]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Constraint] = {
    case (TypeInfo(ThisClass, _), json) => json match {
      case v @ JObject(JField(name, JArray(rawValues)) :: _) =>
        val values = rawValues.map {
          case JString(v) => v
          case x => throw new MappingException("Can't convert " + x + " to String in " + v)
        }.toSet
        Constraint(name, values)
      case x => throw new MappingException("Can't convert " + x + " to " + ThisClass.getName)
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case v: Constraint =>
      import JsonDSL._
      (v.name -> v.values.toList.sorted)
  }
}

class IdSerializer extends Serializer[Id] {
  private val ThisClass = classOf[Id]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Id] = {
    case (TypeInfo(ThisClass, _), json) => json match {
      case v @ JString(value) =>
        Id(value)
      case x => throw new MappingException("Can't convert " + x + " to " + ThisClass.getName)
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case v: Id =>
      import JsonDSL._
      v.value
  }
}

class HashSerializer extends Serializer[Hash] {
  private val ThisClass = classOf[Hash]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Hash] = {
    case (TypeInfo(ThisClass, _), json) => json match {
      case v @ JString(value) =>
        Hash(value)
      case x => throw new MappingException("Can't convert " + x + " to " + ThisClass.getName)
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case v: Hash =>
      import JsonDSL._
      v.value
  }
}

class MetadataSerializer extends Serializer[Metadata] {
  private val ThisClass = classOf[Metadata]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Metadata] = {
    case (TypeInfo(ThisClass, _), json) => json match {
      case v @ JObject(JField(name, JArray(rawValues)) :: _) =>
        val values = rawValues.map {
          case JString(v) => v
          case x => throw new MappingException("Can't convert " + x + " to String in " + v)
        }.toSet
        Metadata(name, values)
      case x => throw new MappingException("Can't convert " + x + " to " + ThisClass.getName)
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case v: Metadata =>
      import JsonDSL._
      v.name -> v.values.toSeq.sorted
  }
}

//TODO: I am not sure, but the AdeptMetadata doesn't work with COnfigurationId?
class ConfigurationIdSerializer extends Serializer[ConfigurationId] {
  private val ThisClass = classOf[ConfigurationId]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), ConfigurationId] = {
    case (TypeInfo(ThisClass, _), json) => json match {
      case v @ JString(value) =>
        ConfigurationId(value)
      case x => throw new MappingException("Can't convert " + x + " to " + ThisClass.getName)
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case v: ConfigurationId =>
      import JsonDSL._
      v.value
  }
}

class CommitSerializer extends Serializer[Commit] {
  private val ThisClass = classOf[Commit]

  def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Commit] = {
    case (TypeInfo(ThisClass, _), json) => json match {
      case v @ JString(value) =>
        Commit(value)
      case x => throw new MappingException("Can't convert " + x + " to " + ThisClass.getName)
    }
  }

  def serialize(implicit formats: Formats): PartialFunction[Any, JValue] = {
    case v: Commit =>
      import JsonDSL._
      v.value
  }
}
