package adept.core.models.internal

private[adept] case class Node(val id: String, var children: Set[Node]) {
  override def toString = {
    id + " <children>"
  }
}
