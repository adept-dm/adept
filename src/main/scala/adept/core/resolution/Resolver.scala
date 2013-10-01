package adept.core.resolution

import adept.core.models._
import scala.annotation.tailrec

case class Node(val id: String, var children: Set[Node])

/* 
 * State is mutable which speeds up the Resolver and makes the code easier to read. 
 * 
 * This will have to be rewritten to make it: 
 *  1) thread-safe for external libraries 
 *  2) multi-threaded to speed up IO (in particular) and CPU bounded operations 
 * */
/*
case class State(
  var allNodes: Map[String, Node] = Map.empty,
  var allVariants: Map[String, Set[Variant]] = Map.empty,
  var visitedVariantsConstraints: Set[(Variant, Set[Constraint])] = Set.empty,

  var resolved: Set[String] = Set.empty,
  var unresolved: Set[String] = Set.empty,
  var counter: Int = 0, //TODO: REMOVE

  var globalConstraints: Map[String, Set[Constraint]] = Map.empty)
*/

class State(
  var resolved: Set[String] = Set.empty,
  var overconstrained: Set[String] = Set.empty,
  var underconstrained: Set[String] = Set.empty,
  var forcedVariants: Map[String, Variant] = Map.empty,
  var resolvedVariants: Map[String, Variant] = Map.empty,

  var globalConstraints: Map[String, Set[Constraint]] = Map.empty) {
  override def toString = {
    "resolved: " + resolved + "\n" +
      "over-constrained: " + overconstrained + "\n" +
      "under-constrained: " + underconstrained + "\n" +
      "resolved-variants: " + resolvedVariants + "\n" +
      "forced-variants: " + forcedVariants + "\n" +
      "globalConstraints: " + globalConstraints + "\n"
  }

  def copy() = {
    new State(
      resolved = resolved,
      underconstrained = underconstrained,
      overconstrained = overconstrained,
      resolvedVariants = (resolvedVariants.keys zip resolvedVariants.values).toMap, //FIXME: hack to make a new copy of values
      forcedVariants = (forcedVariants.keys zip forcedVariants.values).toMap, //FIXME: hack to make a new copy of values
      globalConstraints = (globalConstraints.keys zip globalConstraints.values).toMap) //FIXME: hack to make a new copy of values
  }

  def copy(forcedVariants: Map[String, Variant]) = {
    new State(
      resolved = resolved,
      underconstrained = underconstrained,
      overconstrained = overconstrained,
      forcedVariants = forcedVariants,
      globalConstraints = globalConstraints)
  }
}

class Resolver(variantsLoader: VariantsLoaderEngine) extends VariantsLoaderLogic {

  private[adept] var states = Set.empty[State]

  /*
  private def getResolved(variants: Set[Variant]) = {
    if (isResolved(variants)) variants.headOption
    else None
  }

  private def isResolved(variants: Set[Variant]) = variants.size == 1
  private def isUnderConstrained(variants: Set[Variant]) = variants.size > 1
  private def isOverConstrained(variants: Set[Variant]) = variants.size < 1

  def isResolved(state: State) = {
    state.unresolved.size == 1
  }

  def isCyclic(variant: Variant, constraints: Set[Constraint], state: State) = {
    state.visitedVariantsConstraints.exists(_ == variant -> constraints)
  }


  private def setResolved(id: String, constraints: Set[Constraint], variant: Variant, node: Node, state: State) = {
    if (!isCyclic(variant, constraints, state)) {
      state.resolved += id
      state.unresolved -= id
      state.visitedVariantsConstraints += variant -> constraints
      val (children, _) = resolveDependencies(variant.dependencies, state)
      node.children = children
    }
  }
  */
  /*
  private def setUnresolved(id: String, node: Node, state: State) = {
    state.unresolved += id
    state.resolved -= id
    node.children = Set.empty
  }
*/
  /*private def resolveUnderConstrained(variant: Variant, variants: Set[Variant], state: State): Option[State] = {
    val constraints = state.globalConstraints(variant.moduleId)
    if (variants.nonEmpty && variantsLoader.matches(variant.attributes, constraints)) {
      variant.dependencies.foreach { dependencies =>
        resolveNode(dependencies, Set(variant), state)
      }
      if (state.unresolved.isEmpty) Some(state)
      else {
        val otherVariants = variants - variant
        otherVariants.toList match {
          case (head :: Nil) => resolveUnderConstrained(head, otherVariants, state)
          case _ => None
        }
      }
    } else None
  }
*/
  /*
  private def resolveDependencies(dependencies: Seq[Dependency], state: State): (Set[Node], Seq[State]) = {
    val (nodes, states) = dependencies.map { dependency => //TODO: does foreach + move load in here make this tailrec?
      val constraints = getConstraints(dependency, state)
      state.globalConstraints += dependency.id -> constraints
      val variants = getVariants(dependency.id, constraints)
      resolveNode(dependency, dependencies, variants, state)
    }.unzip

    nodes.toSet -> states
  }
  private def resolveNode(dependency: Dependency, dependencies: Seq[Dependency], variants: Set[Variant], state: State): (Node, State) = {
    state.counter += 1
    val id = dependency.id
    val constraints = getConstraints(dependency, state)
    println("RESOLVING " + id + " in " + dependencies)
    
    val node = state.allNodes.getOrElse(id, {
      val node = Node(id, Set.empty)
      state.allNodes += id -> node
      node
    })

    val branchedState: Option[State] = getResolved(variants) match {
      case Some(variant) =>
        setResolved(id, constraints, variant, node, state)
        //println("2SUCCESS: " + id + "resolved" + state.resolved + " V  " + state.unresolved)
        //println(state.counter)
        None
      case None =>
        if (isUnderConstrained(variants)) {
          /*
          val resolvedBranchedVariants = variants.toList.flatMap { variant =>
            val resolver = new Resolver(variantsLoader, Some(state.copy()))
            println("dipping into " + dependency)
            resolver.resolveNode(dependency, Set(variant))
            if (resolver.isResolved) {
              Some(variant -> resolver)
            } else None
          }
          resolvedBranchedVariants match {
            case ((variant, resolver)) :: Nil =>
              state = resolver.state
              println("resolved id" + id)
              //println(id + variant.attributes.map( a => Constraint(a.name, a.values) ))
              //setResolved(id, variant.attributes.map( a => Constraint(a.name, a.values) ), variant, node)
            case _ => 
              setUnresolved(id, node)
          }
        */
          val branchedStates = variants.toList.map { variant =>
            val branchedState = state.copy()
            println("UNDERCONSTRAINED " + id)
            resolveNode(dependency, dependencies, Set(variant), branchedState)
            branchedState
          }
          branchedStates.filter(_.unresolved.isEmpty) match {
            case branchedState :: Nil =>
              //println("SUCCESS in branched: " + id + "resolved" + branchedState.resolved + " V  " + branchedState.unresolved)
              //println(branchedState.counter)

              setUnresolved(id, node, state)
              //println("Diving into" + dependencies)
              
              resolveDependencies(dependencies.filter(_ != dependency), branchedState)
              Some(branchedState)
            //this.state = state
            case _ =>
              setUnresolved(id, node, state)
              None
          }
        } else {
          setUnresolved(id, node, state)
          None
        }
    }
    branchedState match {
      case Some(foundBranchedState) =>
        foundBranchedState.allVariants += id -> variants
        states ++= Seq(foundBranchedState)
        node -> foundBranchedState
      case None =>
        state.allVariants += id -> variants
        node -> state
    }
  }
  
  private def resolveDependencies(dependencies: Seq[Dependency], state: State): (Set[Node], Seq[State]) = {
    val (nodes, states) = dependencies.map { dependency => //TODO: does foreach + move load in here make this tailrec?
      val constraints = getConstraints(dependency, state)
      state.globalConstraints += dependency.id -> constraints
      val variants = getVariants(dependency.id, constraints)
      resolveNode(dependency, dependencies, variants, state)
    }.unzip

    nodes.toSet -> states
  }*/

  private def getConstraints(dependency: Dependency, state: State) = {
    dependency.constraints ++ state.globalConstraints.get(dependency.id).getOrElse(Set.empty)
  }

  private def getVariants(id: String, state: State) = {
    state.forcedVariants.get(id) match {
      case Some(variant) => Set(variant)
      case None => variantsLoader.get(id, state.globalConstraints(id))
    }
  }

  private def resolveVariants(id: String, variants: Set[Variant], state: State) = {
    if (variants.size == 1) { // exactly one variant == resolved
      val variant = variants.head

      state.resolved += id
      state.underconstrained -= id
      state.overconstrained -= id
      state.resolvedVariants += id -> variant

      resolveDependencies(variant.dependencies, state) // continue

    } else if (variants.size > 1) { // under-constrained
      state.underconstrained += id
      state.resolved -= id
    } else if (variants.size < 1) { // over-constrained
      state.overconstrained += id
      state.resolved -= id
    } else {
      throw new Exception("unexpected amount of variants: " + variants.size + " in " + variants)
    }
  }

  def resolveDependencies(dependencies: Seq[Dependency], state: State): Unit = {
    dependencies.foreach { dependency: Dependency =>
      val id = dependency.id
      val constraints = getConstraints(dependency, state)
      state.globalConstraints += id -> constraints

      val variants = getVariants(dependency.id, state)

      resolveVariants(id, variants, state)
    }
  }

  def variantCombinations(ids: Set[String], state: State): Set[Set[Variant]] = {
    if (ids.isEmpty) Set(Set.empty)
    else {
      for {
        id <- ids
        rest = ids - id
        combination <- variantCombinations(rest, state)
        variant <- getVariants(id, state)
      } yield {
        combination + variant
      }
    }
  }

  def resolve(dependencies: Seq[Dependency]) = { //TODO: Seq should be Set
    def resolve(depedencies: Seq[Dependency], state: State, checkedCombinations: Set[Set[Variant]]): Option[State] = {
      resolveDependencies(dependencies, state)

      if (state.overconstrained.size == 0 && state.underconstrained.size > 0) {
        //under-constrained, but not over-constrained, so try to find the first set of variants that can resolve this:
        val allVariantCombinations = variantCombinations(state.underconstrained, state)

        //amount of combinations required to iterate over: state.underconstrained.size * allVariantCombinations
        val combinationSize = state.underconstrained.size

        val combinationSets = for { //the ordered sequence of each size of combinations to try
          size <- 1 to combinationSize
          combination <- allVariantCombinations.toList
        } yield {
          for {
            variantList <- combination.toList.combinations(size) //TODO: implement combinations of sets?
            variantSet = variantList.toSet
          } yield {
            variantSet
          }
        }

        //view makes it possible to map then find for each combination 
        val resolvedStates = combinationSets.view map { combinations => //find first set of combinations which...
          val states = combinations.toList.flatMap { combination => //...has only one combination that resolves
            if (checkedCombinations(combination)) None
            else {
              val forcedVariants = combination.groupBy(_.moduleId).map {
                case (id, variants) =>
                  if (variants.size != 1) throw new Exception("expected exactly one variant but got: " + variants)
                  else id -> variants.head
              }
              val alreadyForced = (state.forcedVariants.keys.toList.intersect(forcedVariants.keys.toList)).size > 0

              if (alreadyForced) {
                None //we have already tried forcing some of these variants before so skip
              } else {
                val forcedState = state.copy(forcedVariants = forcedVariants)
                val r = resolve(dependencies, forcedState, checkedCombinations + combination)
                r
              }
            }
          }

          states
        } find { resolvedStates =>
          resolvedStates.size == 1
        }

        resolvedStates match {
          case Some(found :: Nil) => Some(found) //we found one resolved state for all current combinations
          case _ => None //none or more than one resolved states means we failed
        }
      } else if (state.underconstrained.size == 0 && state.overconstrained.size == 0 ) {
        Some(state)
      } else None
    }
    
    val initState = new State()
    val result = resolve(dependencies, initState, Set.empty) match {
      case Some(state) => {
        println("FOUND DEEEPLY resolved state: " + state)
        state
      }
      case None => initState
    }

    println(result)

  }

  /*
   * for {
              //never force a variant twice
              variantCombination <- combination
              forcedVariants: Map[String, Variant] = variantCombination.groupBy(_.moduleId).map {
                case (id, variants) =>
                  if (variants.size != 1) throw new Exception("expected exactly one variant but got: " + variants)
                  else id -> variants.head
              }
              forcedState = state.copy(forcedVariants = forcedVariants)
              newState <- {
                resolve(dependencies, forcedState) //, checkedCombinations + variantCombination)
              }
            } yield newState
   * */

}

