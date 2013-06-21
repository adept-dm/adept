package adept.core.models

object Coordinates {
  val CoordsExpr = """\s*(.*):(.*):(.*)\s*""".r

  
  def parse(string: String): Either[String, Coordinates] = {
    string match {
      case CoordsExpr(org, name, version) => {
        if (org.contains(" ") || name.contains(" ") || version.contains(" "))
          Left("could not parse coordinates because of whitespace in expression: '"+string+"'")
        else
          Right(Coordinates(org, name, version))
      }
      case noCoords => Left("could not parse coordinates: "+noCoords)
    }
  }
}

case class Coordinates(org: String, name: String, version: String)
