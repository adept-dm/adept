package adept.repository.models.serialization

case class DeserializationException(errors: Seq[String]) extends Exception {
  val errorsString = errors.mkString(";")
  "Failed with " + (if (errors.size > 1) "multiple errors" else "error") + ": " + errorsString 
}