package adept.ext

import adept.core.models._
import adept.core.resolution.VariantsLoaderEngine
import adept.core.resolution.VariantsLoaderLogic

class DefinedVariants(variants: Seq[Variant], override val logic: VariantsLoaderLogic = new VariantsLoaderLogic) extends VariantsLoaderEngine(logic) {
  val variantsById = variants.groupBy(_.id)

  override def get(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    logic.filter(id, variantsById.get(id).getOrElse(Seq.empty).toSet, constraints)
  }
}
