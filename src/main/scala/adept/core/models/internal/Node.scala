package adept.core.models.internal

import adept.core.models.Id

private[adept] case class Node(val id: Id, var children: Set[Node]) {
  override def toString = {
    id + " <children>"
  }
}
