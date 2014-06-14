package adept.repository.metadata

import adept.artifact.models._
import adept.repository.Repository
import adept.repository.models.Commit
import adept.repository.GitRepository
import java.io.{ByteArrayOutputStream, File, InputStream}
import collection.JavaConverters._
import com.fasterxml.jackson.core.{JsonParser, JsonToken, JsonFactory}

case class ArtifactMetadata(size: Long, locations: Set[ArtifactLocation]) {
  def toArtifact(hash: ArtifactHash): Artifact = {
    new Artifact(hash, size, locations.asJava)
  }

  lazy val jsonString = toJson

  def toJson = {
    val os = new ByteArrayOutputStream()
    val generator = new JsonFactory().createGenerator(os)
    generator.writeStartObject()
    generator.writeNumberField("size", size)
    generator.writeArrayFieldStart("locations")
    for (location <- locations) {
      generator.writeStartObject()
      generator.writeStringField("value", location.value)
      generator.writeEndObject()
    }
    generator.writeEndArray()
    generator.writeEndObject()
    generator.close()
    val json = os.toString
    json
  }

  def write(hash: ArtifactHash, repository: Repository): File = {
    val file = repository.ensureArtifactFile(hash)
    MetadataContent.write(jsonString, file)
  }
}

object ArtifactMetadata {

  def fromArtifact(artifact: Artifact): ArtifactMetadata = {
    ArtifactMetadata(artifact.size, Set() ++ artifact.locations.asScala)
  }

  def read(hash: ArtifactHash, repository: Repository): Option[ArtifactMetadata] = {
    val file = repository.getArtifactFile(hash)
    repository.usingFileInputStream(file) {
      case Right(Some(is)) =>
        readJson(hash, repository, is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read file: " + file.getAbsolutePath + " for hash: " + hash + ". Got error: " + error)
    }
  }

  /** Parse a JSON array into a set.
    *
    * @param parser JSON parser
    * @param converter lambda to convert each element into an object
    * @tparam T element type
    * @return Resulting set
    */
  private def parseSet[T](parser: JsonParser, converter: (JsonParser) => T): Set[T] = {
    if (parser.getCurrentToken != JsonToken.START_ARRAY) {
      throw new Exception("Current token is not array start")
    }

    // Read contents of array
    val array = collection.mutable.Buffer[T]()
    while (parser.nextToken() != JsonToken.END_ARRAY) {
      array += converter(parser)
    }

    array.toSet
  }

  /** Parse a JSON object description.
   *
   * @param is input stream
   * @param parseField lambda to parse each field
   * @return JSON string
   */
  private def parseJson(is: InputStream, parseField: (JsonParser, String) => Unit): String = {
    val json = io.Source.fromInputStream(is).getLines().mkString("\n")
    val parser = new JsonFactory().createParser(json)
    try {
      // Get START_OBJECT
      parser.nextToken()

      // Read field name or END_OBJECT
      while (parser.nextToken() != JsonToken.END_OBJECT) {
        val fieldName = parser.getCurrentName
        // Read value, or START_OBJECT/START_ARRAY
        parser.nextToken()
        parseField(parser, fieldName)
      }
    }
    finally {
      parser.close()
    }

    json
  }

  private def readJson(hash: ArtifactHash, repository: Repository, is: InputStream): Option[ArtifactMetadata] = {
    var size = -1L
    var locations: Option[Set[ArtifactLocation]] = null
    val json = parseJson(is, (parser: JsonParser, fieldName: String) => {
        fieldName match {
          case "size" =>
            size = parser.getLongValue
          case "locations" =>
            locations = Some(parseSet(parser, (parser: JsonParser) => {
              var value: Option[String] = null
              // Read START_OBJECT
              parser.nextToken()
              // Read field name or END_OBJECT
              while (parser.nextToken() != JsonToken.END_OBJECT) {
                val fieldName = parser.getCurrentName
                fieldName match {
                  case "value" =>
                    value = Some(parser.getValueAsString)
                }
              }

              new ArtifactLocation(value.get)
            }))
        }
    })

    if (size == -1 || !locations.isDefined) {
      throw new Exception(s"Invalid JSON: $json")
    }

    Some(ArtifactMetadata(size, locations.get))
    //    Json.fromJson[ArtifactMetadata](json) match {
    //      case JsSuccess(value, _) => Some(value)
    //      case JsError(errors) => throw new Exception("Could not parse json: " + hash + " in dir:  " + repository.dir + " (" + repository.getArtifactFile(hash).getAbsolutePath + "). Got errors: " + errors)
    //    }
  }

  def read(hash: ArtifactHash, repository: GitRepository, commit: Commit): Option[ArtifactMetadata] = {
    repository.usingArtifactInputStream(hash, commit) {
      case Right(Some(is)) =>
        readJson(hash, repository, is)
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read: " + hash + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)
    }
  }
}
