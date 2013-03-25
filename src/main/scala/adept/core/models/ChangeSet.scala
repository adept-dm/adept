package adept.core.models

case class ChangeSet(commit: Commit, changes: Seq[Change]) {
  def toJson = {
    import org.json4s.Extraction._
    implicit val formats = org.json4s.DefaultFormats
    import org.json4s.native.JsonMethods._
    import org.json4s.JsonDSL._
    
    decompose(this)
  }
}

object ChangeSet {
  import org.json4s._
    
  def fromJson(json: JValue): Seq[ChangeSet] = {
    import org.json4s.Extraction._
    implicit val formats = org.json4s.DefaultFormats
    import org.json4s.native.JsonMethods._
    import org.json4s.JsonDSL._
    
    json.extract[List[ChangeSet]]
  }
}