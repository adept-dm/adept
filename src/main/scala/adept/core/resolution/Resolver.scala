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

  private[adept] def combinationSets(underconstrained: Set[String], state: State, checkedCombinations: Set[Set[Variant]]) = {
    val allVariantCombinations = variantCombinations(underconstrained, state)
    val combinationSize = underconstrained.size

    //TODO: limit to a max number of paths to try?
    // or warning if large amount of combinations required to iterate over: 
    // underconstrained.size * allVariantCombinations

    val combinationSets = for { //the ordered sequence of each size of combinations to try, starting with the least amount of variants to constrain
      size <- (1 to combinationSize).toList
      combination <- allVariantCombinations.toList if !checkedCombinations(combination)
    } yield {
      val combinations = (for {
        variantList <- combination.toList.combinations(size).toList //TODO: implement combinations of sets?
      } yield {
        variantList.toSet
      })
      
      //FIXME: we sort (later) and start with the least amount of of forced variants
      // many combinations (should fix variantCombinations!!! instead)
      // it is used for grouping later on
      if (size == 1) size -> List(combinations.flatten.toSet)
      else size -> combinations
    }
    //TODO: add possibility to remove combinations that we will discard later (for example: lower point versions)
    
    combinationSets
  }

  def resolve(dependencies: Set[Dependency], initState: Option[State] = None): Either[State, State] = {
    var checkedCombinations = Set.empty[Set[Variant]] //FIXME: this is currently the reason why we cannot be .par
    var optimalUnderconstrainedStates = Set.empty[State]
    var forcedVariantsUsed = Int.MaxValue

    def forceVariants(combinations: List[Set[Variant]], state: State, previouslyUnderConstrained: Set[String]): List[Option[(State, Set[Node])]] = {
      combinations.map { combination =>
        if (checkedCombinations(combination)) {
          None
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
          if (alreadyForced) None
          else {
            val newConstraints = (for {
              (id, variant) <- forcedVariants
              dependency <- variant.dependencies
            } yield dependency.id -> dependency.constraints).toMap

            val overconstrained = { //check if there exists a forced variant which has constraints that matches another forced variant

              val currentConstraints = dependencies.map { dependency =>
                dependency.id -> (dependency.constraints ++ newConstraints.getOrElse(dependency.id, Set.empty))
              }.toMap

              (state.forcedVariants ++ forcedVariants).exists {
                case (id, variant) if currentConstraints.isDefinedAt(id) =>
                  !matches(variant.attributes, currentConstraints(id) ++ state.constraints.getOrElse(id, Set.empty))
                case _ => false
              }
            }
            //            println()
            if (!overconstrained) {
              val forcedState = state.copy(forcedVariants = forcedVariants)
              forcedState.constraints ++= newConstraints
              //              println("harmonized forced variants: " + forcedVariants)
              resolve(dependencies ++ forcedVariants.flatMap { case (_, variant) => variant.dependencies }, forcedState, previouslyUnderConstrained)
            } else {
              //              println("over-constrained forced variants: " + forcedVariants)
              None
            }
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

        //TODO: grouping and sorting does not feel optimal at all! we already have the size so it should not be necessary + plus we should add it to a sorted list 
        //group by sizes, for each size we want to check if there is a unique resolve before continuing
        val resolvedStates = combinationSets(underconstrained, state, checkedCombinations).groupBy(_._1).toList.sortBy(_._1) map { //TODO: add .view  (removed because we use resolvedStates again later) is here to avoid mapping over solutions where we have already found one
          case (size, combinationsSizes) =>
            combinationsSizes.toList.flatMap {
              case (_, combinations) =>
                val resolvedStates = forceVariants(combinations, initState, underconstrained).toList
                resolvedStates.collect {
                  case res if res.isDefined => res
                }
            }
        }

        val chosenState = resolvedStates find { combinationStates =>
          combinationStates.size == 1 //we found exactly one combination
        }

        chosenState match {
          case Some(Some(resolvedState) :: Nil) =>
            Some(resolvedState) //we found exactly one resolved state for all current combinations
          case _ =>
            //we found more than one possible states:
            resolvedStates.foreach { a => 
              val b = a.collect { //use only the ones that resolve 
                case Some((state, nodes)) =>
                  state.graph = nodes
                  state
              }

              //...then store the states that used the least amount of forced variants to resolve
              b.foreach { c =>
                if (c.forcedVariants.size == forcedVariantsUsed) {
                  optimalUnderconstrainedStates += c
                } else if (c.forcedVariants.size < forcedVariantsUsed) {
                  forcedVariantsUsed = c.forcedVariants.size
                  optimalUnderconstrainedStates = Set(c)
                }
              }
            }

            None //none or more than one resolved states so we failed
        }
      } else if (state.underconstrained.size == 0 && state.overconstrained.size == 0) {
        Some(state -> nodes)
      } else {
        None
      }
    }

    val currentState = initState.getOrElse(new State())
    resolve(dependencies, currentState, Set.empty) match {
      case Some((state, nodes)) => { //found a state that resolves
        state.graph = nodes //setting root nodes
        Right(state)
      }
      case None =>
        println("===========================")
        println(optimalUnderconstrainedStates)
        println("===========================")
        Left(currentState) //found no path to resolution, so return first graph for debug
    }
  }
}

