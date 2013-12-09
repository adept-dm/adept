package adept.core.models

case class Node(val id: Id, val children: Set[Node]) {
  override def toString = {
    id + " <children>"
  }
}
