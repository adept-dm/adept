package adept.models

object Visibility extends Enumeration {
  val Public = Value
  val Private = Value
}

//TODO:case class Configuration(name: String, description: Option[String], extendsFrom: Set[String], visibility: Visibility.Value, deprecated: Option[String])
case class Configuration(name: String, description: Option[String], extendsFrom: Set[String], deprecated: Option[String])
