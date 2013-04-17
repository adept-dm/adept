package adept.models

import java.io.File
import org.json4s.JValue

case class Module(
    coords: Coordinates,
    artifact: Artifact,
    dependencies: Set[Dependency]
)

object Module {
  def read(json: JValue): Seq[Module] = {
    import org.json4s.Extraction._
    implicit val formats = org.json4s.DefaultFormats
    import org.json4s.native.JsonMethods._
    import org.json4s.JsonDSL._
    json.extract[List[Module]]
  }
  
}