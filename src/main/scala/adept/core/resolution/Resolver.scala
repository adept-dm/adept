package adept.core.resolution

import adept.core.models._

class Resolver(variantsLoader: VariantsLoaderEngine) extends VariantsLoaderLogic {
  case class Node(val id: String, var children: Set[Node])

  def isResolved(variants: Set[Variant]) = variants.size == 1
  def isCyclic(variant: Variant, constraints: Set[Constraint]) = {
    visitedVariantsConstraints.exists(_ == variant -> constraints)
  }

  /* 
   * Using vars here, speeds up and makes the code easier to read. 
   * 
   * This will have to be rewritten to make it: 
   *  1) thread-safe for external libraries 
   *  2) multi-threaded to speed up IO (in particular) and CPU bounded operations 
   * */
  private[adept] var allNodes = Map.empty[String, Node]
  private[adept] var allVariants = Map.empty[String, Set[Variant]]
  private[adept] var visitedVariantsConstraints = Set.empty[(Variant, Set[Constraint])]

  private[adept] var resolved = Set.empty[String]
  private[adept] var unresolved = Set.empty[String]

  private[adept] var globalConstraints = Map.empty[String, Set[Constraint]]

  def load(dependencies: Seq[Dependency]): Set[Node] = {
    dependencies.map { dependency =>
      val id = dependency.id
      val node = allNodes.getOrElse(id, {
        val node = Node(id, Set.empty)
        allNodes += id -> node
        node
      })

      val newConstraints = dependency.constraints ++ globalConstraints.get(id).flatten
      globalConstraints += id -> newConstraints

      val variants = variantsLoader.get(id, newConstraints)

      if (isResolved(variants)) {
        resolved += id
        unresolved -= id

        var foundCyclic = false
        val children = variants.filter { variant =>
          val cyclic = isCyclic(variant, newConstraints)
          if (cyclic && !foundCyclic) foundCyclic = true
          !cyclic
        }.flatMap { variant =>
          visitedVariantsConstraints += variant -> newConstraints
          load(variant.dependencies)
        }
        if (!foundCyclic) node.children = children
      } else {
        unresolved += id
        resolved -= id

        node.children = Set.empty
      }

      allVariants += id -> variants
      node
    }.toSet
  }
}

/* Results
case class Node(variant: Variant, constraints: Set[Constraint], children: Seq[Node])
trait GraphLike
class PartialGraph(nodes: Set[Node]) extends GraphLike

trait Result

trait ResolveFailure
case class OverConstrained(ids: Set[(String, Set[(String, Set[Constraint])])], graph: PartialGraph) extends ResolveFailure
case class UnderConstrained(variants: Map[String, Set[Variant]], graph: PartialGraph) extends ResolveFailure

trait Success
case class Graph(nodes: Set[Node]) extends Success with GraphLike
case class Artifacts(artifacts: Set[Artifact]) extends Success
*/

