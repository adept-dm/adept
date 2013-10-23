package adept.test

import adept.core.models._
import adept.core.resolution.VariantsLoaderEngine

class DefinedVariants(variants: Seq[Variant]) extends VariantsLoaderEngine {
  val variantsById = variants.groupBy(_.id)

  override def get(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    filter(id, variantsById.get(id).getOrElse(Seq.empty).toSet, constraints)
  }
}
