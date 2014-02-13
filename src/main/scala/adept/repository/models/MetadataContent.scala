package adept.repository.models

import adept.models.Artifact
import java.io.Reader
import java.io.BufferedReader
import adept.repository.models.serialization.DeserializationException
import play.api.libs.json.JsResult
import play.api.libs.json.JsValue
import java.io.Writer
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import play.api.libs.json.Json

private[adept] object MetadataContent {
  def readString(reader: Reader) = {
    val bufferedReader = new BufferedReader(reader)
    try {
      var lines = new StringBuilder
      var line = bufferedReader.readLine()
      while (line != null) {
        lines.append(line)
        line = bufferedReader.readLine()
      }
      lines.toString
    } finally {
      bufferedReader.close()
    }
  }

  def fromJson[A](reader: Reader)(validate: JsValue => JsResult[A]): A = {
    import play.api.libs.json._
    validate(Json.parse(readString(reader))).fold(
      errors => throw new DeserializationException(errors
        .map {
          case (jsPath, validationErrors) =>
            "Failed at: " + jsPath.toString + " with " + validationErrors.mkString(" and ")
        }),
      value => value)
  }

  //Writers ---

  def writeString(writer: Writer, content: String) = {
    val bufferedWriter = new BufferedWriter(writer)
    try {
      bufferedWriter.write(content)
      bufferedWriter.flush()
    } finally {
      bufferedWriter.close()
    }
  }

  private[models] def usingFileWriter[A](file: File)(func: FileWriter => Unit): File = {
    val fileWriter = new FileWriter(file)
    try {
      func(fileWriter)
      file
    } finally {
      fileWriter.close()
    }
  }
}

case class MetadataContent(variantsMetadata: Set[ConfiguredVariantsMetadata], artifactsMetadata: Set[Artifact])