package adept.core.models

//TODO: unique id support
case class Dependency(coordinates: Coordinates, hash: Hash, configuration: String, isTransitive: Boolean = true, exclusionRules: Set[DependencyExclusionRule] = Set.empty)

object Dependency {
  import org.json4s._
  import adept.utils.JsonHelpers._
  import org.json4s.JsonDSL._

  def readDependency(json: JValue): Either[String, Dependency] = {
    for {
      hash <- (eitherOf[String](json) \ "hash").right
      configuration <- (eitherOf[String](json) \ "configuration").right
      isTransitive <- getOptionalValue[Boolean](json, "transitive").right
      coords <- Coordinates.readCoords(json).right
      exclusionRules <- DependencyExclusionRule.readDependencyExclusionRules(json \ "exclusion-rules").right
    } yield {
     Dependency(coords, Hash(hash), configuration, isTransitive.getOrElse(true), exclusionRules)
    }
  }

  def readDependencies(json: JValue): Either[String, Set[Dependency]] = {
    readSet(json) { f =>
      f.map(readDependency)
    }
  }

  def dependencyToJson(dep: Dependency): JValue = {
    val maybeTransitive = if (!dep.isTransitive) Some(false) else None
    Coordinates.coordsToJson(dep.coordinates) ~
    asJObject(List[JField](
        ("hash" -> dep.hash.value),
      ("configuration" -> dep.configuration),
      ("transitive" -> maybeTransitive),
      ("exclusion-rules" -> dep.exclusionRules.map( DependencyExclusionRule.toJson ))
      ).map(ifNonEmpty): _*)
  }
}