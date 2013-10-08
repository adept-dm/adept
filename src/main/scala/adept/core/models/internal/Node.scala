package adept.core.models.internal

private[core] case class Node(val id: String, var children: Set[Node]) {
  override def toString = {
    id + " <children>"
  }
}
