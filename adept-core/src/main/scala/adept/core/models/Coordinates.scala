package adept.core.models

object Coordinates {
  import org.json4s._
  import adept.utils.JsonHelpers._
  import org.json4s.JsonDSL._

  val CoordsExpr = """\s*(.*):(.*):(.*)\s*""".r

  def readCoords(json: JValue): Either[String, Coordinates] = {
    for {
      org <- (eitherOf[String](json) \ "organization").right
      name <- (eitherOf[String](json) \ "name").right
      version <- (eitherOf[String](json) \ "version").right
    } yield {
      Coordinates(org, name, version)
    }
  }

  def coordsToJson(coords: Coordinates) = {
    ("organization" -> coords.org) ~
      ("name" -> coords.name) ~
      ("version" -> coords.version)
  }

  def parse(string: String): Either[String, Coordinates] = {
    string match {
      case CoordsExpr(org, name, version) => {
        if (org.contains(" ") || name.contains(" ") || version.contains(" "))
          Left("could not parse coordinates because of whitespace in expression: '" + string + "'")
        else
          Right(Coordinates(org, name, version))
      }
      case noCoords => Left("could not parse coordinates: " + noCoords)
    }
  }
}

case class Coordinates(org: String, name: String, version: String) {
  override def toString = org + ":" + name + ":" + version
}
