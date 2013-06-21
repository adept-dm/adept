package adept.core.models

import adept.utils.Logging

object Visibility extends Enumeration {
  val Public = Value
  val Private = Value
}

case class Configuration(name: String, description: Option[String], extendsFrom: Set[String], visibility: Visibility.Value, deprecated: Option[String])
