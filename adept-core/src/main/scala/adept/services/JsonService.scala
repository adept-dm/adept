package adept.services

import com.fasterxml.jackson.core._
import java.io.{InputStream, ByteArrayOutputStream}
import scala.io.Source
import adept.artifact.models.JsonSerializable
import scala.collection.mutable
import java.text.SimpleDateFormat
import java.util.Date
import adept.exceptions.JsonMissingFieldException

case class ValueMap(map: mutable.Map[String, Any] = mutable.Map.empty[String, Any]) {
  def add(key: String, value: Any): Unit = map(key) = value

  def get[T](key: String): T = map.getOrElse(key, throw JsonMissingFieldException(
    key)).asInstanceOf[T]

  val getString = get[String] _

  val getStringSeq = getSeq[String] _

  val getStringSet = getSet[String] _

  def getStringSeqMap = get[Map[String, Seq[String]]] _

  def getOption[T](key: String): Option[T] = map.get(key).asInstanceOf[Option[T]]

  def getOrElse[T](key: String, default: T): T = map.getOrElse(key, default).asInstanceOf[T]

  def getSeq[T](key: String): Seq[T] = get[Seq[T]](key)

  def getSet[T](key: String): Set[T] = get[Set[T]](key)
}

object JsonService {
  def writeJson(converter: (JsonGenerator) => Unit): String = {
    writeTopLevel({generator =>
      generator.writeStartObject()
      converter(generator)
      generator.writeEndObject()
    })
  }

  def writeJsonArray(objects: Iterable[JsonSerializable]): String = {
    writeTopLevel({generator =>
      generator.writeStartArray()
      for (obj <- objects) {
        writeObject(obj, generator)
      }
      generator.writeEndArray()
    })
  }

  private def writeTopLevel(converter: (JsonGenerator) => Unit): String = {
    val os = new ByteArrayOutputStream()
    val generator = new JsonFactory().createGenerator(os)
    generator.useDefaultPrettyPrinter()
    try {
      converter(generator)
    }
    finally {
      generator.close()
    }

    val json = os.toString
    json
  }

  def writeObjectField[T](fieldName: String, map: Map[String, Seq[String]],
                          generator: JsonGenerator) {
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

  private def writeObject(value: JsonSerializable, generator: JsonGenerator) {
    generator.writeStartObject()
    value.writeJson(generator)
    generator.writeEndObject()
  }

  def writeObject(fieldName: String, value: JsonSerializable, generator: JsonGenerator) {
    generator.writeObjectFieldStart(fieldName)
    writeObject(value, generator)
  }

  def writeObject(fieldName: String, value: Option[JsonSerializable], generator: JsonGenerator) {
    if (value.isDefined) {
      writeObject(fieldName, value.get, generator)
    }
  }

  def writeStringArrayField(fieldName: String, values: Iterable[String],
                            generator: JsonGenerator) {
    generator.writeArrayFieldStart(fieldName)
    for (elem <- values) {
      generator.writeString(elem)
    }
    generator.writeEndArray()
  }

  def writeArrayField(fieldName: String, values: Iterable[JsonSerializable],
                      generator: JsonGenerator) {
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

  /** Parse a JSON document from an input stream.
    *
    * @param is input stream
    * @param field2converter map from field names to lambdas for converting to values
    * @param constructor lambda to construct object from field values
    * @tparam T object type
    * @return parsed object and JSON document
    */
  def parseJson[T](is: InputStream, field2converter: Map[String, (JsonParser) => Any],
                   constructor: ValueMap => T): (T, String) = {
    val json = Source.fromInputStream(is).getLines().mkString("\n")
    val parser = new JsonFactory().createParser(json)
    try {
      // Get START_OBJECT
      parser.nextToken()
      (parseObject(parser, field2converter, constructor), json)
    }
    finally {
      parser.close()
    }
  }

  /** Parse a JSON object.
    *
    * @param parser parser instance
    * @param field2converter map from field names to lambdas for converting to values
    * @param constructor lambda to construct object from field values
    * @tparam T object type
    * @return parsed object
    */
  def parseObject[T](parser: JsonParser, field2converter: Map[String, JsonParser => Any],
                     constructor: ValueMap => T): T = {
    parseObjectReal(parser, (parser, fieldName) => field2converter(fieldName)(parser), constructor)
  }

  private def parseObjectReal[T](parser: JsonParser, fieldConverter:
  (JsonParser, String) => Any, constructor: ValueMap => T): T = {
    assert(parser.getCurrentToken == JsonToken.START_OBJECT)
    val valueMap = ValueMap()
    // Read field name or END_OBJECT
    while (parser.nextToken() != JsonToken.END_OBJECT) {
      assert(parser.getCurrentToken == JsonToken.FIELD_NAME)
      val fieldName = parser.getCurrentName
      // Read value, or START_OBJECT/START_ARRAY
      parser.nextToken()

      valueMap.add(fieldName, fieldConverter(parser, fieldName))
    }

    constructor(valueMap)
  }

  /** Parse a JSON array into a sequence.
    *
    * @param parser JSON parser
    * @param converter lambda to convert each element into an object
    * @tparam T element type
    * @return Resulting sequence
    */
  def parseSeq[T](parser: JsonParser, converter: (JsonParser) => T): Seq[T] = {
    assert(parser.getCurrentToken == JsonToken.START_ARRAY)

    // Read contents of array
    val array = collection.mutable.Buffer[T]()
    while (parser.nextToken() != JsonToken.END_ARRAY) {
      array += converter(parser)
    }
    array.toSeq
  }

  def parseSet[T](parser: JsonParser, converter: (JsonParser) => T): Set[T] = {
    parseSeq(parser, converter).toSet
  }

  def parseStringSet(parser: JsonParser): Set[String] = {
    parseSet(parser, _.getValueAsString)
  }

  def parseStringSeq(parser: JsonParser): Seq[String] = {
    parseSeq(parser, _.getValueAsString)
  }

  def parseStringSeqMap(parser: JsonParser): Map[String, Seq[String]] = {
    val map = scala.collection.mutable.Map[String, Seq[String]]()
    parseObjectReal(parser, (parser: JsonParser, fieldName: String) => {
      parseStringSeq(parser)
    }, _.map map { case (key, value) => (key, value.asInstanceOf[Seq[String]])})

    map.toMap
  }
  
  def parseDate(parser: JsonParser, dateFormat: SimpleDateFormat): Date = {
    dateFormat.parse(parser.getValueAsString)
  }
}
