package adept.test

import adept.core.models._
import adept.core.resolution.VariantsLoaderEngine

class DefinedVariants(variants: Seq[Variant]) extends VariantsLoaderEngine {
  val variantsById = variants.groupBy(_.moduleId)

  override def get(id: String, constraints: Set[Constraint]): Set[Variant] = {
    filter(id, variantsById.get(id).flatten.toSet, constraints)
  }
}
