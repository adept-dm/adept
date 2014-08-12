package adept.repository

import adept.resolution.models._

/**
 * This rather boring class just loads the variants it has in memory
 * 
 * It is useful for testing.
 */
class MemoryLoader(private[adept] val variants: Set[Variant]) extends VariantsLoader {
  val variantsById = variants.groupBy(_.id) //avoid filtering ids that we know won't match

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    AttributeConstraintFilter.filter(id, variantsById.getOrElse(id, Set.empty).toSet, constraints)
  }

}
