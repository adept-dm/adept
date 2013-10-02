package adept.core.resolution

import adept.core.models._
import scala.annotation.tailrec

/* 
 * State is mutable which speeds up the Resolver and makes the code easier to read. 
 * TODO: I am not sure we even need to have vars here now...
 * 
 * This will have to be rewritten to make it: 
 *  1) thread-safe for external libraries 
 *  2) multi-threaded to speed up IO (in particular) and CPU bounded operations 
 * */
class State(
  var nodes: Map[String, Node] = Map.empty,
  var graph: Set[Node] = Set.empty,
  var visited: Set[Set[Variant]] = Set.empty,

  var resolved: Set[String] = Set.empty,
  var overconstrained: Set[String] = Set.empty,
  var underconstrained: Set[String] = Set.empty,
  var forcedVariants: Map[String, Variant] = Map.empty,
  var resolvedVariants: Map[String, Variant] = Map.empty,

  var constraints: Map[String, Set[Constraint]] = Map.empty) {
  override def toString = {
    var printedIds = Set.empty[String]
    def nodesToString(nodes: Set[Node], level: Int): String = {
      nodes.foreach(printedIds += _.id)
      nodes.map { n =>
        val (cyclic, nonCyclic) = n.children.partition(n => printedIds(n.id))
        val cyclicString = cyclic.map(n => (" " * (level + 1)) + "- " + n.id + " <defined>").mkString("\n")
        val nonCyclicString = nodesToString(nonCyclic, level + 1)
        (" " * level) + "- " + resolvedVariants(n.id) + (if (cyclicString.isEmpty) "" else "\n" + cyclicString + "") + (if (nonCyclicString.isEmpty) "" else ("\n" + nonCyclicString)) 
      }.mkString("\n")
    }

    "resolved: " + resolved + "\n" +
      "over-constrained: " + overconstrained + "\n" +
      "under-constrained: " + underconstrained + "\n" +
      "resolved-variants: " + resolvedVariants + "\n" +
      "forced-variants: " + forcedVariants + "\n" +
      "constraints: " + constraints + "\n" +
      "graph:\n" + nodesToString(graph, 0)
  }

  def copy() = {
    new State(
      //NOTE TO SELF: please fix FIXMEs they are ugly as hell :)
      nodes = (nodes.keys zip nodes.values).toMap, //FIXME: hack to make a new copy of values
      graph = graph,
      visited = visited,
      resolved = resolved,
      underconstrained = underconstrained,
      overconstrained = overconstrained,
      resolvedVariants = (resolvedVariants.keys zip resolvedVariants.values).toMap, //FIXME: hack to make a new copy of values
      forcedVariants = (forcedVariants.keys zip forcedVariants.values).toMap, //FIXME: hack to make a new copy of values
      constraints = (constraints.keys zip constraints.values).toMap) //FIXME: hack to make a new copy of values
  }

  def copy(forcedVariants: Map[String, Variant]) = {
    new State(
      nodes = (nodes.keys zip nodes.values).toMap, //FIXME: hack to make a new copy of values
      graph = graph,
      visited = visited,
      resolved = resolved,
      underconstrained = underconstrained,
      overconstrained = overconstrained,
      forcedVariants = forcedVariants,
      constraints = constraints)
  }
}

case class Node(val id: String, var children: Set[Node]) {
  override def toString = {
    id + " <children>"
  }
}

class Resolver(variantsLoader: VariantsLoaderEngine) extends VariantsLoaderLogic {

  private[adept] def getVariants(id: String, state: State) = {
    state.forcedVariants.get(id) match {
      case Some(variant) => Set(variant)
      case None => variantsLoader.get(id, state.constraints(id))
    }
  }

  private[adept] def resolveVariants(id: String, variants: Set[Variant], state: State): Node = {
    val node = state.nodes.getOrElse(id, Node(id, Set.empty))
    state.nodes += id -> node

    if (!state.visited(variants)) {
      state.visited += variants
      if (variants.size == 1) { // exactly one variant == resolved
        val variant = variants.head

        state.resolved += id
        state.underconstrained -= id
        state.overconstrained -= id
        state.resolvedVariants += id -> variant

        val children = resolveDependencies(variant.dependencies, state) // continue
        node.children = children
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
    node
  }

  private[adept] def resolveDependencies(dependencies: Set[Dependency], state: State): Set[Node] = {
    dependencies.map { dependency: Dependency =>
      val id = dependency.id
      val constraints = dependency.constraints ++ state.constraints.get(dependency.id).getOrElse(Set.empty)
      state.constraints += id -> constraints

      val variants = getVariants(dependency.id, state)

      resolveVariants(id, variants, state)
    }
  }

  private[adept] def variantCombinations(ids: Set[String], state: State): Set[Set[Variant]] = {
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

  def resolve(dependencies: Set[Dependency], initState: Option[State] = None): Either[State, State] = {
    
    def detectResolvedStates(combinations: List[Set[Variant]], state: State, previouslyUnderConstrained: Set[String], checkedCombinations: Set[Set[Variant]]): List[Option[(State, Set[Node])]] = {
      combinations.map { combination => //...has only one combination that resolves
        if (checkedCombinations(combination)) {
          None //TODO: we avoid stackoverflows here, but I am not sure it is the right thing to do. the reasoning is that we checked this combination earlier in this graph and it didn't resolve so no point in trying again?
        } else {
          val forcedVariants = combination.groupBy(_.moduleId).map {
            case (id, variants) =>
              if (variants.size != 1) throw new Exception("expected exactly one variant but got: " + variants)
              else id -> variants.head
          }
          //check if we are trying to force a module that is already forced
          val alreadyForced = state.forcedVariants.exists {
            case (id1, variant1) =>
              forcedVariants.exists {
                case (id2, variant2) =>
                  (id1 == id2) && (variant1 != variant2)
              }
          }

          if (false) None
          else {
            val forcedState = state.copy(forcedVariants = forcedVariants)
            resolve(dependencies, forcedState, previouslyUnderConstrained, checkedCombinations + combination)
          }
        }
      }
    }

    def resolve(depedencies: Set[Dependency], state: State, previouslyUnderConstrained: Set[String], checkedCombinations: Set[Set[Variant]]): Option[(State, Set[Node])] = {
      val initState = state.copy() //saving state, in case we need a new try
      val nodes = resolveDependencies(dependencies, state)

      if (state.overconstrained.size == 0 && state.underconstrained.size > 0) {
        //under-constrained, but not over-constrained, so try to find the first set of variants that can resolve this:
        //basically it involves finding all possible combinations of variants, then trying to find the smallest amount of the variants that unambiguously resolves
        val underconstrained = state.underconstrained ++ previouslyUnderConstrained

        val combinationSets = {
          val allVariantCombinations = variantCombinations(underconstrained, state) //using state and not initState to avoid getting too many variants (we want to prune out constraints)
          //warning if large amount of combinations required to iterate over: state.underconstrained.size * allVariantCombinations
          //TODO: limit to a max number of paths to try?
          val combinationSize = underconstrained.size
          val combinationSets = for { //the ordered sequence of each size of combinations to try, starting with the least amount of variants to constrain
            size <- (1 to combinationSize).toList
            combination <- allVariantCombinations.toList
          } yield {
            val combinations = (for {
              variantList <- combination.toList.combinations(size).toList //TODO: implement combinations of sets?
            } yield {
              variantList.toSet
            })
            //FIXME: trying to avoid too many combinations (should fix variantCombinations!!! instead)
            if (size == 1) size -> List(combinations.flatten.toSet)
            else size -> combinations
          }
          combinationSets
        }
        
        //TODO: grouping and sorting does not feel optimal at all! we already have the size so it should not be necessary + plus we should add it to a sorted list 
        //group by sizes, for each size we want to check if there is a unique resolve before continuing
        //ALSO NOTICE THE .par!!! gives a very nice perf boost, but single core performance sucks - something is wrong, there must be some ways to skip some combinations
        val resolvedStates = combinationSets.groupBy(_._1).toList.sortBy(_._1).par.view map { //view is here to avoid mapping over solutions where we have already found one
          case (size, combinationsSizes) =>
            combinationsSizes.toList.flatMap {
              case (_, combinations) =>
                val resolvedStates = detectResolvedStates(combinations, initState, underconstrained, checkedCombinations).toList
                resolvedStates.collect {
                  case res if res.isDefined => {
                    res
                  }
                }
            }
        }

        val chosenState = resolvedStates find { combinationStates =>
          combinationStates.size == 1
        }

        chosenState match {
          case Some(Some(resolvedState) :: Nil) => Some(resolvedState) //we found exactly one resolved state for all current combinations
          case _ =>
            None //none or more than one resolved states so we failed
        }
      } else if (state.underconstrained.size == 0 && state.overconstrained.size == 0) {
        Some(state -> nodes)
      } else None
    }

    val currentState = initState.getOrElse(new State())
    resolve(dependencies, currentState, Set.empty, Set.empty) match {
      case Some((state, nodes)) => { //found a state that resolves
        state.graph = nodes //setting root nodes
        Right(state)
      }
      case None => Left(currentState) //found no path to resolution, so return first graph for debug
    }
  }
}

