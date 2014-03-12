package adept.repository

import adept.resolution.models._

/**
 * Can load (or conjure if you will) variants from anywhere such as memory or one or more Git repository.
 */
trait VariantsLoader {
  /** Load the variants matching id and constraints from where-ever they are stored  */
  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant]
}