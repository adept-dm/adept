package adept.core.resolution

import adept.core.models._

case class Node(val id: String, var children: Set[Node])

/* 
 * State is mutable which speeds up the Resolver and makes the code easier to read. 
 * 
 * This will have to be rewritten to make it: 
 *  1) thread-safe for external libraries 
 *  2) multi-threaded to speed up IO (in particular) and CPU bounded operations 
 * */
case class State(
  var allNodes: Map[String, Node] = Map.empty,
  var allVariants: Map[String, Set[Variant]] = Map.empty,
  var visitedVariantsConstraints: Set[(Variant, Set[Constraint])] = Set.empty,

  var resolved: Set[String] = Set.empty,
  var unresolved: Set[String] = Set.empty,

  var globalConstraints: Map[String, Set[Constraint]] = Map.empty)

class Resolver(variantsLoader: VariantsLoaderEngine, initState: Option[State] = None) extends VariantsLoaderLogic {

  private def getResolved(variants: Set[Variant]) = {
    if (isResolved(variants)) variants.headOption
    else None
  }

  private def isResolved(variants: Set[Variant]) = variants.size == 1
  private def isUnderConstrained(variants: Set[Variant]) = variants.size > 1
  private def isOverConstrained(variants: Set[Variant]) = variants.size < 1

  def isResolved = state.unresolved.isEmpty

  def isCyclic(variant: Variant, constraints: Set[Constraint]) = {
    state.visitedVariantsConstraints.exists(_ == variant -> constraints)
  }

  private[adept] var state = initState.getOrElse(State())

  private def setResolved(id: String, constraints: Set[Constraint], variant: Variant, node: Node) = {
    if (!isCyclic(variant, constraints)) {
      state.resolved += id
      state.unresolved -= id
      state.visitedVariantsConstraints += variant -> constraints
  
      node.children = resolve(variant.dependencies)
    }
  }

  private def setUnresolved(id: String, node: Node) = {
    state.unresolved += id
    state.resolved -= id
    node.children = Set.empty
  }

  def resolveNode(dependency: Dependency, variants: Set[Variant]): Node = {
    val id = dependency.id
    val constraints = getConstraints(dependency)
    
    val node = state.allNodes.getOrElse(id, {
      val node = Node(id, Set.empty)
      state.allNodes += id -> node
      node
    })

    getResolved(variants) match {
      case Some(variant) =>
        setResolved(id, constraints, variant, node)
      case None =>
        if (isUnderConstrained(variants)) {
          val resolvedBranchedVariants = variants.toList.flatMap { variant =>
            val resolver = new Resolver(variantsLoader, Some(state.copy()))
            resolver.resolveNode(dependency, Set(variant))
            if (resolver.isResolved) {
              Some(variant -> resolver)
            } else None
          }
          resolvedBranchedVariants match {
            case ((variant, resolver)) :: Nil =>
              state = resolver.state
            case _ => setUnresolved(id, node)
          }
        } else setUnresolved(id, node)
    }

    state.allVariants += id -> variants
    node
  }

  private def getConstraints(dependency: Dependency) = {
    dependency.constraints ++ state.globalConstraints.get(dependency.id).flatten
  }

  private def getVariants(id: String, constraints: Set[Constraint]) = {
    variantsLoader.get(id, constraints)
  }

  def resolve(dependencies: Seq[Dependency]) = {

    def resolve(dependencies: Seq[Dependency]): Set[Node] = {
      dependencies.map { dependency => //TODO: does foreach + move load in here make this tailrec?
        val constraints = getConstraints(dependency)
        state.globalConstraints += dependency.id -> constraints
        val variants = getVariants(dependency.id, constraints)
        resolveNode(dependency, variants)
      }.toSet
    }

    resolve(dependencies)
  }

}

