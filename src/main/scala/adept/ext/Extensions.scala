package adept.ext

import adept.core.resolution.Resolver
import adept.core.models._
import adept.core.models.internal._

case class Query(exprs: (String, String)*)

object Query {
  def matches(variant: Variant, query: Query): Boolean = {
    val attributes = variant.attributes
    val constraints = query.exprs
    val zipped = query.exprs.map { case expr @ (exprName, _) =>
      expr -> attributes.filter(attribute => attribute.name == exprName)
    }
    zipped.forall{ case ((exprName, exprValue), matchingAttributes) =>
      matchingAttributes.exists{ attribute =>
        exprValue match {
          case "*" => true
          case _ =>
            attribute.values.contains(exprValue)
        }
      }
    }
  }
}

object Extensions {

  def exclude(graph: Set[Node], variants: Map[String, Variant], query: Query): (Set[Variant], Map[String, Variant]) = {
    //FIXME: vars...
    var includedVariants = Map.empty[String, Variant]
    var newVariants = Set.empty[Variant]

    def exclude(nodes: Set[Node]): Unit = {
      nodes.foreach { node =>
        val variant = variants(node.id)
        val excludedDependencies = variant.dependencies.filter { dependency =>
          val dependentVariant = variants(dependency.id)
          if (Query.matches(dependentVariant, query)) {
            true
          } else {
            false
          }
        }
        val includedVariant = if (excludedDependencies.nonEmpty) {
          val newVariant = variant.copy(dependencies = variant.dependencies.filter(d => !excludedDependencies.contains(d)))
          newVariants += newVariant
          newVariant
        } else variant
        
        includedVariants += node.id -> includedVariant
        
        val excludedIds = excludedDependencies.map(_.id)

        exclude(node.children.filter { child => //exclude all children
          !excludedIds.contains(child.id)
        })
      }
    }
    exclude(graph)
    (newVariants, includedVariants)
  }

  def overrides() = {
    //for all dependencies with overrides (dependencies)
    //find all variants that depends on something that matches query (query)
    //clone variants with new set of dependencies and attributes (overridden)
    //return new variants and dependencies to new variants
  }
}
