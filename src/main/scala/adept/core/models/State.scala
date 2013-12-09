package adept.core.models

case class State(
  val underconstrained: Set[Id],
  val overconstrained: Set[Id],
  val resolved: Set[Id],
  val resolvedVariants: Map[Id, Variant],
  val implicitVariants: Map[Id, Variant],
  val constraints: Map[Id, Set[Constraint]],
  val nodes: Map[Id, Node]) {

  def isResolved = underconstrained.size == 0 && overconstrained.size == 0
  def isUnderconstrained = underconstrained.size > 0 && overconstrained.size == 0
  def isOverconstrained = overconstrained.size > 0

  override def toString = {
    "resolved: " + resolved + "\n" +
      (if (overconstrained.nonEmpty) "over-constrained: " + overconstrained + "\n" else "") +
      (if (underconstrained.nonEmpty) "under-constrained: " + underconstrained + "\n" else "") +
      "resolved-variants: " + resolvedVariants + "\n" +
      "implicit-variants: " + implicitVariants + "\n" +
      "constraints: " + constraints
  }
}