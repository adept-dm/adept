package adept.core.resolution

import adept.core.models._
import adept.core.models.internal._

class Resolver(variantsLoader: VariantsLoaderEngine) {

  /**
   * Calculate all possible combinations of variants that should be implicit
   * Starts with the simplest combination (one and one variant), then continues with pairs, etc etc
   *
   * TODO: return iterators to be lazy and avoid calculating the more complicated combinations
   */
  def combinations(ids: Set[Id], ignoredIds: Set[Id], constraints: Map[Id, Set[Constraint]]): Iterator[Iterator[List[Variant]]] = {

    val variants = ids.filter { id =>
      !ignoredIds(id) //ignore ids: can be either implicit already or under-constrained 
    }.flatMap { id =>
      variantsLoader.get(id, constraints.getOrElse(id, Set.empty)) //only add a combination of something that is not constrained
    }

    (1 to ids.size).iterator.map { size =>
      variants.toList.combinations(size)
    }
  }

  def resolveVariant(dependency: Dependency, state: State): (Option[Variant], State) = {
    val id = dependency.id
    state.implicitVariants.get(id) match {
      case Some(variant) =>
        Some(variant) -> state
      case None =>
        val currentConstraints = dependency.constraints ++ state.constraints.getOrElse(id, Set.empty)

        val variants = variantsLoader.get(id, currentConstraints)
        if (variants.size == 1) { //resolved
          val variant = variants.head //TODO: use pattern match instead

          Some(variant) -> state.copy(
            resolved = state.resolved + id,
            resolvedVariants = state.resolvedVariants + (id -> variant),
            underconstrained = state.underconstrained - id,
            overconstrained = state.overconstrained - id,
            constraints = state.constraints + (id -> currentConstraints))
        } else if (variants.size > 1) { //under-constrained
          None -> state.copy(
            resolved = state.resolved - id,
            resolvedVariants = state.resolvedVariants - id,
            underconstrained = state.underconstrained + id,
            overconstrained = state.overconstrained - id,
            constraints = state.constraints + (id -> currentConstraints))
        } else if (variants.size < 1) { //over-constrained
          None -> state.copy(
            resolved = state.resolved - id,
            resolvedVariants = state.resolvedVariants - id,
            underconstrained = state.underconstrained - id,
            overconstrained = state.overconstrained + id,
            constraints = state.constraints + (id -> currentConstraints))
        } else {
          throw new Exception("Unexpected number of variants: " + variants)
        }
    }
  }

  def resolveDependencies(dependencies: Set[Dependency], visited: Set[Dependency], lastState: State): State = {
    val newDependencies = dependencies.filter { dependency =>
      !visited(dependency) //remove dependencies we have already visited
    }

    //TODO: evaluate whether breadth-first is normally faster than depth-first (used now) or find an heuristic that is good at _finding constraints_ that are _likely_ to be used: I believe this is what it is all about 
    newDependencies.foldLeft(lastState) { (state, dependency) => //collapse all dependencies that can be found into one
      resolveVariant(dependency, state) match {
        case (Some(variant), resolvedState) =>
          if (variant.dependencies.isEmpty) resolvedState
          else resolveDependencies(variant.dependencies, visited + dependency, resolvedState)
        case (None, unresolvedState) if !unresolvedState.isResolved =>
          unresolvedState
        case _ => throw new Exception("Could not find a variant for a resolved state")
      }
    }
  }

  def resolve(dependencies: Set[Dependency]) = {
    val initState = new State(
      resolved = Set.empty,
      underconstrained = Set.empty,
      overconstrained = Set.empty,
      resolvedVariants = Map.empty,
      implicitVariants = Map.empty,
      constraints = Map.empty)

    implicitResolve(dependencies, initState, Set.empty)
  }

  private def implicitResolve(dependencies: Set[Dependency], currentState: State, previouslyUnderconstrained: Set[Id]): Either[State, State] = {

    val state = resolveDependencies(dependencies, Set.empty, currentState)

    if (state.isUnderconstrained) {
      //under-constrained; perhaps there is a unique combination of variants where we still can resolve:

      val nonImplicitDependencies = dependencies.filter { dependency =>
        !state.implicitVariants.isDefinedAt(dependency.id)
      }

      val ignoredIds = (state.implicitVariants.values.map { variant => //ignore id of implicit variants
        variant.id
      } ++ previouslyUnderconstrained).toSet //ignore ids which are under-constrained already

      //try out the different combinations till we find a unique combination that resolves
      val testedStatesCombinations = combinations(state.underconstrained, ignoredIds, state.constraints).map { combinations =>
        //TODO: .par to improve speed?
        combinations.map { combination =>
          val implicitVariants = combination.map { variant =>
            variant.id -> variant
          }.toMap

          val implicitState = state.copy(
            underconstrained = state.underconstrained -- combination.map(_.id), //we are no longer under-constrained on the implicitVariants
            implicitVariants = state.implicitVariants ++ implicitVariants)
          implicitResolve(nonImplicitDependencies, implicitState, ignoredIds ++ state.underconstrained) //ignore ids that are already under-constrained at this level
        }.collect {
          case Right(state) => state
        }.toList //TODO: is there a way to avoid this toList? We need to use this iterator to find then extract, which is why it is there
      }
      testedStatesCombinations find (_.size > 0) match {
        case Some(resolvedState +: Nil) => Right(resolvedState)
        case None => Left(state)
        case result =>
          println("=====================")
          println(result.map(_.mkString("\n\n")))
          println("=====================")
          Left(state)
      }
    } else if (state.isOverconstrained) {
      Left(state)
    } else if (state.isResolved) {
      Right(state)
    } else {
      throw new Exception("State is neither resolved, underconstrained nor overconstrained: " + state)
    }

  }
}

