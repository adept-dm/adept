package adept.core.resolution

import adept.core.models._

trait VariantsLoaderEngine extends VariantsLoaderLogic {
  def get(id: String, constraints: Set[Constraint]): Set[Variant]
}
