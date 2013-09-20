package adept.core.resolution

import adept.core.models._

trait VariantsLoaderLogic {
  def matches(attributes: Set[Attribute], constraints: Set[Constraint]): Boolean = {
    val zipped = constraints.map { constraint =>
      constraint -> attributes.filter(attribute => attribute.name == constraint.name)
    }
    val mismatch = zipped.exists {
      case (constraint, matchingAttributes) =>
        matchingAttributes.isEmpty || {
          matchingAttributes.exists { attribute =>
            val intersecting = attribute.values.intersect(constraint.values)
            intersecting.isEmpty
          }
        }
    }
    !mismatch
  }

  def conlicts(constraints: Set[Constraint]) = constraints.groupBy(_.name).exists { case (name, constraints) => constraints.size > 1 }

  def filter(id: String, variants: Set[Variant], constraints: Set[Constraint]): Set[Variant] = {
    if (conlicts(constraints)) Set.empty
    else variants.filter { variant =>
      assert(id == variant.moduleId)
      matches(variant.attributes, constraints)
    }
  }
}
