package adept.core.models


case class Dependency(coordinates: Coordinates, override val uniqueId: Option[UniqueId], configuration: String, force: Boolean = false, isTransitive: Boolean = true, exclusionRules: Set[DependencyExclusionRule] = Set.empty) extends DependencyDescriptor(coordinates.org, coordinates.name, coordinates.version, uniqueId) 

object Dependency {
  import org.json4s._
  import adept.utils.JsonHelpers._
  import org.json4s.JsonDSL._

  def readDependency(json: JValue): Either[String, Dependency] = {
    for {
      uniqueIdOpt <- getOptionalValue[String](json, "unique-id").right
      configuration <- (eitherOf[String](json) \ "configuration").right
      isTransitive <- getOptionalValue[Boolean](json, "transitive").right
      force <- getOptionalValue[Boolean](json, "force").right
      coords <- Coordinates.readCoords(json).right
      exclusionRules <- DependencyExclusionRule.readDependencyExclusionRules(json \ "exclusion-rules").right
    } yield {
      Dependency(coords, uniqueIdOpt.map(UniqueId(_)), configuration, force.getOrElse(false), isTransitive.getOrElse(true), exclusionRules)
    }
  }

  def readDependencies(json: JValue): Either[String, Set[Dependency]] = {
    readSet(json) { f =>
      f.map(readDependency)
    }
  }

  def dependencyToJson(dep: Dependency): JValue = {
    val maybeTransitive = if (!dep.isTransitive) Some(false) else None
    val maybeForce = if (dep.force) Some(true) else None

    Coordinates.coordsToJson(dep.coordinates) ~
      asJObject(List[JField](
        ("unique-id" -> dep.uniqueId.map(_.value)),
        ("configuration" -> dep.configuration),
        ("force" -> maybeForce),
        ("transitive" -> maybeTransitive),
        ("exclusion-rules" -> dep.exclusionRules.map(DependencyExclusionRule.toJson))).map(ifNonEmpty): _*)
  }
}