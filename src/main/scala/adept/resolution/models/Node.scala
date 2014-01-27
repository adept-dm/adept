package adept.resolution.models

import adept.models.Id

case class Node(val id: Id, val children: Set[Node]) {
  override def toString = {
    id + " <children>"
  }
}
