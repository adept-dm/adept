package adept.resolution.resolver.models

import adept.resolution.models.Constraint
import adept.resolution.models.Id
import adept.resolution.models.Variant

/**
 * Contains the computed state after resolution.
 * 
 * `resolvedVariants` are the variants which are resolved (duh).
 * 
 * `implicitVariants` are the variants which could not be resolved, but 
 * could be implicitly inferred to be correct because only this actual variant
 * made the graph resolve.
 * 
 * The result of both `resolvedVariants` and `implicitVariants` are the variants
 * which was resolved. 
 * 
 * State is immutable and thread-safe.
 */
case class State(
  val underconstrained: Set[Id],
  val overconstrained: Set[Id],
  val resolved: Set[Id],
  val resolvedVariants: Map[Id, Variant],
  val implicitVariants: Map[Id, Variant],
  val excluded: Set[Id],
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
      "excluded: " + excluded + "\n" +
      "constraints: " + constraints
  }
}