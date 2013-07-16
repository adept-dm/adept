package adept.core.models

case class Override(override val organization: String, override val name: String, overriddenVersion: String,  override val uniqueId: Option[UniqueId]) extends DependencyDescriptor(organization, name, overriddenVersion, uniqueId)

object Override {
  import org.json4s._
  import adept.utils.JsonHelpers._
  import org.json4s.JsonDSL._

  def readOverride(json: JValue): Either[String, Override] = {
    for {
      org <- (eitherOf[String](json) \ "organization").right
      name <- (eitherOf[String](json) \ "name").right
      overriddenVersion <- (eitherOf[String](json) \ "override-version").right
      uniqueIdOpt <- getOptionalValue[String](json, "unique-id").right
    } yield {
      Override(org, name, overriddenVersion, uniqueIdOpt.map(UniqueId(_)))
    }
  }

  def readOverrides(json: JValue): Either[String, Set[Override]] = {
    readSet(json) { f =>
      f.map(readOverride)
    }
  }

  def overrideToJson(o: Override): JValue = {
    asJObject(List[JField](
      ("organization" -> o.organization),
      ("name" -> o.name),
      ("override-version" -> o.overriddenVersion),
      ("unique-id" -> o.uniqueId.map(_.value))).map(ifNonEmpty): _*)
  }
}