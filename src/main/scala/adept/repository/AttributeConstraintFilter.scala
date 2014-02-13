package adept.repository

import adept.models._

/**
 * Utility object to filter attributes matching a given set of constraints and id
 */
private[adept] object AttributeConstraintFilter {

  /** Returns true if there is there is a constraint that matches the attributes */
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

  protected[adept]def conlicts(constraints: Set[Constraint]) = constraints.groupBy(_.name).exists { case (name, constraints) => constraints.size > 1 }

  /** 
   * Filters variants with the matching constraints AND id
   **/
  def filter(id: Id, variants: Set[Variant], constraints: Set[Constraint]): Set[Variant] = {
    if (conlicts(constraints)) Set.empty
    else variants.filter { variant =>
      id == variant.id && matches(variant.attributes, constraints)
    }
  }
}