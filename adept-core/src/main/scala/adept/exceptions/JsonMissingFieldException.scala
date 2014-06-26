package adept.exceptions

case class JsonMissingFieldException(fieldName: String) extends Exception(
  s"JSON missing field $fieldName") {
}
