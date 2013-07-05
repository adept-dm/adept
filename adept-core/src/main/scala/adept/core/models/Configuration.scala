package adept.core.models

import adept.utils.Logging

object Visibility extends Enumeration {
  val Public = Value
  val Private = Value
}

case class Configuration(name: String, description: Option[String], extendsFrom: Set[String], visibility: Visibility.Value, deprecated: Option[String])

object Configuration {
  import org.json4s._
  import adept.utils.JsonHelpers._
  import org.json4s.JsonDSL._

  def readConfiguration(json: JValue): Either[String, Configuration] = {
    val maybeExtendsFrom = getOptionalStrings(json, "extends")
    val eithers = for {
      name <- (eitherOf[String](json) \ "name").right
      visibilityString <- (eitherOf[String](json) \ "visibility").right
      visibility <- Visibility.values.find(_.toString.toLowerCase == visibilityString).toRight("Could not find visibility: " + visibilityString).right
      extendsFrom <- maybeExtendsFrom.right
    } yield {
      for {
        deprectated <- getOptionalValue[String](json, "deprecated").right
        description <- getOptionalValue[String](json, "description").right
      } yield {
        Configuration(name, description, extendsFrom, visibility, deprectated)
      }
    }
    eithers.joinRight
  }

  def readConfigurations(json: JValue): Either[String, Set[Configuration]] = {
    readSet(json) { f =>
      f.map(readConfiguration)
    }
  }

  def configurationToJson(config: Configuration): JValue = {
    asJObject(List[JField](
      ("name" -> config.name),
      ("description" -> config.description),
      ("visibility" -> config.visibility.toString.toLowerCase),
      ("extends" -> config.extendsFrom),
      ("deprecated" -> config.deprecated)).map(ifNonEmpty): _*)
  }

  def defaultConfigurationMapping(conf: String, defaultConfMapping: String = ""): String = { //maps like: http://ant.apache.org/ivy/history/2.1.0/ivyfile/dependency.html
    if (!conf.contains("->")) {
      if (defaultConfMapping.isEmpty) {
        conf match {
          case "" => "*->*"
          case expr if expr.contains("->") => expr
          case expr => expr + "->" + expr
        }
      } else {
        val allParts = defaultConfMapping.split(";").map { confMapping =>
          val parts = confMapping.split("->")
          if (parts.size > 2) throw new Exception("found more that one -> in " + confMapping)
          val from = parts.head
          val to = parts.last
          from match {
            case "*" if conf.nonEmpty => Some(conf + "->" + to)
            case `conf` => Some(from + "->" + to)
            case _ => None
          }
        }
        val matchingParts = allParts.collect {
          case Some(p) => p
        }

        if (matchingParts.isEmpty) defaultConfMapping
        else matchingParts.mkString(";")
      }
    } else conf
  }
}