package adept.core.resolution

import adept.core.models._

abstract class VariantsLoaderEngine(val logic: VariantsLoaderLogic = VariantsLoaderLogic.default) {
  def get(id: Id, constraints: Set[Constraint]): Set[Variant]
}
