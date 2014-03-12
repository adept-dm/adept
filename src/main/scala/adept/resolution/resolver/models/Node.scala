package adept.resolution.resolver.models

import adept.resolution.models.Id

case class Node(val id: Id, val children: Set[Node]) {
  override def toString = {
    id + " <children>"
  }
}
