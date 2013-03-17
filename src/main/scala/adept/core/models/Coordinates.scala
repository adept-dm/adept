package adept.core.models

case class Coordinates(org: String, name: String, version: String) {
  override def toString = s"$org:$name:$version" 
}