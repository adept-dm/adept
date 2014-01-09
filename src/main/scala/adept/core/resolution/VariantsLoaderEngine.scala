package adept.core.resolution

import adept.core.models._


//TODO: merge logic and engine into one: VariantEngine
abstract class VariantsLoaderEngine(val logic: VariantsLoaderLogic = VariantsLoaderLogic.default) {
  def get(id: Id, constraints: Set[Constraint]): Set[Variant]
}
