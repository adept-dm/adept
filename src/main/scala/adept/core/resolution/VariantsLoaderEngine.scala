package adept.core.resolution

import adept.core.models._

abstract class VariantsLoaderEngine(val logic: VariantsLoaderLogic) {
  def get(id: Id, constraints: Set[Constraint]): Set[Variant]
}
