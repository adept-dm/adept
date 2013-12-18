package adept.ext

import adept.core.resolution.Resolver
import adept.core.models._
import adept.logging.Logging
import java.security.MessageDigest
import java.io._

private[adept] sealed trait Query
private[adept] case class QueryExpr(exprs: Seq[(String, String)]) extends Query
private[adept] case class QueryId(id: Id) extends Query

private[adept] object Query {
  def apply(id: Id) = QueryId(id)
  def apply(exprs: (String, String)*) = QueryExpr(exprs)
  
  def matches(variant: Variant, query: Query): Boolean = {
    query match {
      case QueryExpr(constraints) =>
        val attributes = variant.attributes
        val zipped = constraints.map {
          case expr @ (exprName, _) =>
            expr -> attributes.filter(attribute => attribute.name == exprName)
        }
        zipped.forall {
          case ((exprName, exprValue), matchingAttributes) =>
            matchingAttributes.exists { attribute =>
              exprValue match {
                case "*" => true
                case _ =>
                  attribute.values.contains(exprValue)
              }
            }
        }
      case QueryId(id) => variant.id == id
    }
  }
}

/**
 * @param dependencies the dependencies that needs to be used based on replace
 * @param newVariants the new variants generated
 * @param attributes the attributes that was replaced
 * @includedVariants all the variants used for input
 * @graph the new graph
 *
 */
//TODO: remove unnecessary fields
private[adept] case class ReplaceResult(dependencies: Set[Dependency], newVariants: Set[Variant], attributes: Map[Id, Set[Attribute]], includedVariants: Map[Id, Variant], graph: Set[Node])

private[adept] object Extensions extends Logging {
  import AttributeDefaults._
  
  def mergeAttributes(allAttributes: Set[Attribute]*): Set[Attribute] = {
    allAttributes.flatten.groupBy(_.name).map {
      case (name, attrs) =>
        Attribute(name, attrs.toSet.flatMap((_: Attribute).values))
    }(collection.breakOut)
  }

  def mergeConstraints(allConstraints: Set[Constraint]*): Set[Constraint] = {
    allConstraints.flatten.groupBy(_.name).map {
      case (name, attrs) =>
        Constraint(name, attrs.toSet.flatMap((_: Constraint).values))
    }(collection.breakOut)
  }

  /** excluded attributes looks like this: "exclusions": [ "foo/bar:123aef" ] */
  def excludedAttribute(variant: Variant): Attribute = {
    Attribute(ExclusionAttribute, Set(variant.id + ":" + Hash.calculate(variant)))
  }

  def exclude(baseDependencies: Set[Dependency], graph: Set[Node], variants: Map[Id, Variant], query: Query): ReplaceResult = {
    replaceNode(baseDependencies, graph, variants, query) { (matchingDependencies, variant) =>
      val excludedAttributes = matchingDependencies.map(dependency => excludedAttribute(variants(dependency.id)))
      val newVariant = variant.copy(dependencies = variant.dependencies.filter(d => !matchingDependencies.contains(d)),
        attributes = mergeAttributes(variant.attributes, excludedAttributes))
      excludedAttributes -> newVariant
    }
  }

  def overriddenAttribute(variant: Variant, newDependencies: Set[Dependency]): Attribute = {
    val newDepsHash = Hash.calculate(newDependencies)
    val oldDepsHash = Hash.calculate(variant.dependencies)

    Attribute(OverridesAttribute, Set(variant.id + ":" + oldDepsHash + ":" + newDepsHash))
  }

  //TODO: doc that @param variants are the variants that are already resolved and might have sub dependencies that also should be overridden
  def overrides(baseDependencies: Set[Dependency], graph: Set[Node], variants: Map[Id, Variant], query: Query, replacements: Map[Id, Set[Attribute]]): ReplaceResult = {
    replaceNode(baseDependencies, graph, variants, query) { (matchingDependencies, variant) =>
      val overrideDependencies = variant.dependencies.map { dependency =>
        replacements.get(dependency.id) match {
          case Some(attrs) =>
            val names = attrs.map(_.name)
            val constraints = dependency.constraints.filter(constraint => !names(constraint.name)) ++ attrs.map(_.toConstraint)
            dependency.copy(constraints = constraints)
          case None => dependency
        }
      }
      val overriddenAttributes = matchingDependencies.map(dependency => overriddenAttribute(variants(dependency.id), overrideDependencies))

      val newVariant = variant.copy(dependencies = overrideDependencies,
        attributes = mergeAttributes(variant.attributes, overriddenAttributes))
      overriddenAttributes -> newVariant
    }
  }

  private def replaceNode(replacementDependencies: Set[Dependency], graph: Set[Node], variants: Map[Id, Variant], query: Query)(replaceFun: (Set[Dependency], Variant) => (Set[Attribute], Variant)): ReplaceResult = {
    //FIXME: vars...
    var includedVariants = Map.empty[Id, Variant]
    var newVariants = Set.empty[Variant]
    var attributes = Map.empty[Id, Set[Attribute]]
    var dependencies = Map.empty[Id, Dependency]
    var newDependencies = Set.empty[Dependency]

    val dependencyMap = replacementDependencies.map { dependency =>
      dependency.id -> dependency
    }.toMap

    def transitiveReplace(nodes: Set[Node]): Set[Node] = { //TODO: tailrec??
      nodes.map { node =>
        val variant = variants(node.id)
        //find matching dependencies based on query
        val matchingDependencies = variant.dependencies.filter { dependency =>
          variants.get(dependency.id) match {
            case Some(variant) => Query.matches(variant, query)
            case None => 
              //logger.error(s"Expected to find ${dependency.id} in ${variants.mkString(", ")}")
              false
          }
        }

        //find new variant if it is to be replaced or use old one
        val includedVariant = if (matchingDependencies.nonEmpty) {
          val (replacedAttributes, newVariant) = replaceFun(matchingDependencies, variant)

          newVariants += newVariant
          attributes += node.id -> replacedAttributes

          val newConstraints = replacedAttributes.map(_.toConstraint)
          dependencyMap.get(node.id) match { //found a dependency we have already, so we must add a attributes to it
            case Some(declaredDependency) =>
              dependencies += node.id -> declaredDependency.copy(constraints = mergeConstraints(declaredDependency.constraints, newConstraints))
            case None => newDependencies += Dependency(node.id, newConstraints) //a new dependency with excluded constraints
          }
          newVariant
        } else variant

        includedVariants += node.id -> includedVariant

        val excludedIds = matchingDependencies.map(_.id)

        val children = transitiveReplace(node.children.filter { child =>
          //remove children that we are replacing:
          !excludedIds.contains(child.id) &&
            variants.isDefinedAt(child.id) //include only variants that are defined
        })
        node.copy(id = node.id, children = children)
      }
    }

    val newGraph = transitiveReplace(graph)
    val replacedDependencies = replacementDependencies.map { replacementDependency =>
      dependencies.getOrElse(replacementDependency.id, replacementDependency)
    }
    ReplaceResult(replacedDependencies ++ newDependencies, newVariants, attributes, includedVariants, newGraph)
  }
}
