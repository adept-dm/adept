package adept.core.models

case class Metadata(data: Map[String, String]) {
  override val toString = s"[${data.map(e => s"${e._1}=${e._2}")mkString(",")}]" 
}