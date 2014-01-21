package adept.core.resolution

import adept.core.models._

object VariantsLoaderLogic {
  def default() = new VariantsLoaderLogic
}

/**
 * Defines how constraints and attributes matches
 */
class VariantsLoaderLogic {
  def matches(attributes: Set[Attribute], constraints: Set[Constraint]): Boolean = {
    if (constraints.nonEmpty) {
      val zipped = constraints.map { constraint =>
        constraint -> attributes.filter(attribute => attribute.name == constraint.name)
      }
      val mismatch = zipped.exists {
        case (constraint, matchingAttributes) =>
          matchingAttributes.isEmpty || {
            matchingAttributes.exists { attribute =>
              //a match (not a mismatch) if constraints and attributes are both empty:
              !(attribute.values.isEmpty && constraint.values.isEmpty) &&
                {
                  val intersecting = attribute.values.intersect(constraint.values)
                  //is a mismatch if there are no intersecting values of constrains of attributes
                  intersecting.isEmpty
                }
            }
          }
      }
      !mismatch
    } else { //no constraints, means we match
      true
    }
  }

  def conlicts(constraints: Set[Constraint]) = constraints.groupBy(_.name).exists { case (name, constraints) => constraints.size > 1 }

  def filter(id: Id, variants: Set[Variant], constraints: Set[Constraint]): Set[Variant] = {
    if (conlicts(constraints)) Set.empty
    else variants.filter { variant =>
      assert(id == variant.id, "expected id: " + id + " to be equal to " + variant.id + " in " + variant)
      matches(variant.attributes, constraints)
    }
  }
}
