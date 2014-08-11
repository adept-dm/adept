package adept.exceptions

abstract class JsonParseExceptionBase(message: String) extends Exception(message)

case class JsonParseException(message: String, json: String) extends JsonParseExceptionBase(message)

case class JsonMissingFieldException(fieldName: String) extends JsonParseExceptionBase(
  s"JSON missing field $fieldName")
