package adept.services

import com.fasterxml.jackson.core._
import java.io.{InputStream, ByteArrayOutputStream}
import scala.io
import adept.artifact.models.JsonSerializable

object JsonService {
  def writeJson(converter: (JsonGenerator) => Unit): String = {
    val os = new ByteArrayOutputStream()
    val generator = new JsonFactory().createGenerator(os)
    generator.useDefaultPrettyPrinter()
    try {
      generator.writeStartObject()
      converter(generator)
      generator.writeEndObject()
    }
    finally {
      generator.close()
    }

    val json = os.toString
    json
  }

  def writeObjectField[T](fieldName: String, map: Map[String, Seq[String]], generator: JsonGenerator) {
    generator.writeObjectFieldStart(fieldName)
    map.foreach {
      case (key, values) =>
        generator.writeArrayFieldStart(key)
        for (value <- values) {
          generator.writeString(value)
        }
        generator.writeEndArray()
    }
    generator.writeEndObject()
  }

  def writeObject(value: JsonSerializable, generator: JsonGenerator) {
    generator.writeStartObject()
    value.writeJson(generator)
    generator.writeEndObject()
  }

  def writeObject(value: Option[JsonSerializable], generator: JsonGenerator) {
    if (value.isDefined) {
      writeObject(value.get, generator)
    }
  }

  def writeStringArrayField(fieldName: String, values: Iterable[String], generator: JsonGenerator) {
    generator.writeArrayFieldStart(fieldName)
    for (elem <- values) {
      generator.writeString(elem)
    }
    generator.writeEndArray()
  }

  def writeArrayField(fieldName: String, values: Iterable[JsonSerializable], generator: JsonGenerator) {
    generator.writeArrayFieldStart(fieldName)
    for (value <- values) {
      writeObject(value, generator)
    }
    generator.writeEndArray()
  }

  def writeStringField(fieldName: String, value: Option[String], generator: JsonGenerator) {
    if (value.isDefined) {
      generator.writeStringField(fieldName, value.get)
    }
  }

  /** Parse a JSON array into a sequence.
    *
    * @param parser JSON parser
    * @param converter lambda to convert each element into an object
    * @tparam T element type
    * @return Resulting sequence
    */
  def parseSeq[T](parser: JsonParser, converter: () => T): Seq[T] = {
    assert (parser.getCurrentToken == JsonToken.START_ARRAY)

    // Read contents of array
    val array = collection.mutable.Buffer[T]()
    while (parser.nextToken() != JsonToken.END_ARRAY) {
      array += converter()
    }

    array.toSeq
  }

  def parseSet[T](parser: JsonParser, converter: () => T): Set[T] = {
    parseSeq(parser, converter).toSet
  }

  /** Parse a JSON object description.
    *
    * @param is input stream
    * @param parseField lambda to parse each field
    * @return JSON string
    */
  def parseJson(is: InputStream, parseField: (JsonParser, String) => Unit): String = {
    val json = io.Source.fromInputStream(is).getLines().mkString("\n")
    val parser = new JsonFactory().createParser(json)
    try {
      // Get START_OBJECT
      parser.nextToken()
      parseObject(parser, parseField)
    }
    finally {
      parser.close()
    }

    json
  }

  def parseStringSet(parser: JsonParser): Set[String] = {
    parseSet(parser, parser.getValueAsString)
  }

  def parseStringSeq(parser: JsonParser): Seq[String] = {
    parseSeq(parser, parser.getValueAsString)
  }

  def parseObject(parser: JsonParser, parseField: (JsonParser, String) => Unit): Unit = {
    assert (parser.getCurrentToken == JsonToken.START_OBJECT)
    // Read field name or END_OBJECT
    while (parser.nextToken() != JsonToken.END_OBJECT) {
      assert (parser.getCurrentToken == JsonToken.FIELD_NAME)
      val fieldName = parser.getCurrentName
      // Read value, or START_OBJECT/START_ARRAY
      parser.nextToken()
      parseField(parser, fieldName)
    }
  }

  def parseStringMap(parser: JsonParser): Map[String, Seq[String]] = {
    val map = scala.collection.mutable.Map[String, Seq[String]]()
    parseObject(parser, (parser: JsonParser, fieldName: String) => {
      map(fieldName) = parseStringSeq(parser)
    })

    map.toMap
  }
}
