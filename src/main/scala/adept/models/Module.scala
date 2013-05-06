package adept.models

import java.io.File
import org.json4s.JValue

case class Module(
    coords: Coordinates,
    artifacts: Set[Artifact],
    configurations: Set[Configuration],
    attributes: Map[String, Seq[String]],
    dependencies: Set[Dependency]
) {
  lazy val hash = Hash.mix(artifacts.map(_.hash).toSeq)
}

object Module {
  
  def read(json: JValue): Seq[Module] = {
    import org.json4s.Extraction._
    implicit val formats = org.json4s.DefaultFormats
    import org.json4s.native.JsonMethods._
    import org.json4s.JsonDSL._
    json.extract[List[Module]]
  }
  
}