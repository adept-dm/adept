package adept.core.models

case class Universe(name: String, version: String)

object Universe {
  import org.json4s._
  import adept.utils.JsonHelpers._
  import org.json4s.JsonDSL._

  def readUniverse(json: JValue): Either[String, Universe] = {
    for {
      name <- (eitherOf[String](json) \ "name").right
      version <- (eitherOf[String](json) \ "version").right
    } yield {
      Universe(name, version)
    }
  }

  def readUniverses(json: JValue): Either[String, Set[Universe]] = {
    readSet(json) { f =>
      f.map(readUniverse)
    }
  }

  def universeToJson(universe: Universe) = {
    ("name" -> universe.name) ~
      ("version" -> universe.version)
  }
}