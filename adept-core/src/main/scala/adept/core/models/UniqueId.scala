package adept.core.models

import java.util.Date
import java.text.SimpleDateFormat

case class UniqueId(value: String)

object UniqueId {
  lazy val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
  
  def default(coordinates: Coordinates, artifacts: Set[Artifact]): UniqueId = {
    val hash = Hash.mix(artifacts.map(_.hash).toSeq.sortBy(_.value)) 
    val shortHash = hash.value.slice(0, 7)
    val value = coordinates + "-" + shortHash
    UniqueId(value)
  }
  
  def default(coordinates: Coordinates, created: Date, artifacts: Set[Artifact]): UniqueId = {
    val hash = Hash.mix(artifacts.map(_.hash).toSeq.sortBy(_.value)) 
    val shortHash = hash.value.slice(0, 7)
    val value = coordinates + "-" + dateFormat.format(created) + "-" + shortHash
    UniqueId(value)
  }
}