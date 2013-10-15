package adept.core.resolution

import adept.core.models._
import adept.core.models.internal._


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
    var checkedCombinations = Set.empty[Set[Variant]] //FIXME: this is currently the reason why we cannot be .par
    
    def detectResolvedStates(combinations: List[Set[Variant]], state: State, previouslyUnderConstrained: Set[String]): List[Option[(State, Set[Node])]] = {
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
          checkedCombinations += combination //MUTATE
          if (false) None
          else {
            val forcedState = state.copy(forcedVariants = forcedVariants)
            resolve(dependencies, forcedState, previouslyUnderConstrained)
          }
        }
      }
    }

    def resolve(depedencies: Set[Dependency], state: State, previouslyUnderConstrained: Set[String]): Option[(State, Set[Node])] = {
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
            combination <- allVariantCombinations.toList if !checkedCombinations(combination)
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
        val resolvedStates = combinationSets.groupBy(_._1).toList.sortBy(_._1).view map { //view is here to avoid mapping over solutions where we have already found one
          case (size, combinationsSizes) =>
            combinationsSizes.toList.flatMap {
              case (_, combinations) =>
                val resolvedStates = detectResolvedStates(combinations, initState, underconstrained).toList
                resolvedStates.collect {
                  case res if res.isDefined => res
                }
            }
        }

        val chosenState = resolvedStates find { combinationStates =>
          combinationStates.size == 1
        }

        chosenState match {
          case Some(Some(resolvedState) :: Nil) => Some(resolvedState) //we found exactly one resolved state for all current combinations
          case _ => None //none or more than one resolved states so we failed
        }
      } else if (state.underconstrained.size == 0 && state.overconstrained.size == 0) {
        Some(state -> nodes)
      } else None
    }

    val currentState = initState.getOrElse(new State())
    resolve(dependencies, currentState, Set.empty) match {
      case Some((state, nodes)) => { //found a state that resolves
        state.graph = nodes //setting root nodes
        Right(state)
      }
      case None => Left(currentState) //found no path to resolution, so return first graph for debug
    }
  }
}

