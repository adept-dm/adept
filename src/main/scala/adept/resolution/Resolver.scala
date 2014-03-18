package adept.resolution

import adept.resolution.models._
import adept.resolution.resolver.models._
import adept.repository.VariantsLoader

class UnexpectedResolutionStateException(msg: String) extends Exception(msg)

/**
 * Based on a set of requirements (which are just an id and some constraints)
 * and a `loader`, which loads variants, the `Resolver` can resolve the dependency graph.
 *
 * This means that it will search for a set of variants based on the requirements,
 * where there are exactly one variant per Id.
 *
 * If it finds more than one variant per Id, the `ResolvedResult` is "under-constrained".
 * If it there is no variant that matches the Id and constraints, the `ResolvedResult` is
 * "over-constrained".
 *
 * Usage:
 * {{{
 *
 * val variants: Set[Variant] = ...
 * val memoryVariantsLoader = new MemoryVariantsLoader(variants)
 * val resolver = new Resolver(memoryVariantsLoader)
 *
 * val requirements: Set[Requirement] = ...
 * resolver.resolve(requirements)
 *
 * }}}
 */
class Resolver(loader: VariantsLoader, skipImplicitResolve: Boolean = false) {

  /**
   * Calculate all possible combinations of variants that should be implicit
   * Starts with the simplest combination (one and one variant), then continues with pairs, etc etc
   *
   */
  private[adept] def combinations(ids: Set[Id], ignoredIds: Set[Id], constraints: Map[Id, Set[Constraint]]): Iterator[Iterator[List[Variant]]] = {
    val variants = ids.filter { id =>
      !ignoredIds(id) //ignore ids: can be either implicit or under-constrained already
    }.flatMap { id =>
      loader.loadVariants(id, constraints.getOrElse(id, Set.empty)) //only add a combination of something that is not over-constrained
    }

    (1 to ids.size).iterator.map { size =>
      variants.toList.combinations(size) //remove combinations with more than one of the same id
        .filter(variants => variants.map(_.id).toSet.size == variants.size)
    }
  }

  private[adept] def resolveVariant(requirement: Requirement, state: State): (Option[Variant], State) = {
    val id = requirement.id
    state.implicitVariants.get(id) match {
      case Some(variant) =>
        Some(variant) -> state
      case None =>
        val currentConstraints = requirement.constraints ++ state.constraints.getOrElse(id, Set.empty)

        val variants = loader.loadVariants(id, currentConstraints)
        val node = state.nodes.getOrElse(requirement.id, Node(requirement.id, Set.empty))

        if (variants.size == 1) { //resolved
          val variant = variants.head //TODO: any point in using pattern match instead?

          Some(variant) -> state.copy(
            resolved = state.resolved + id,
            resolvedVariants = state.resolvedVariants + (id -> variant),
            underconstrained = state.underconstrained - id,
            overconstrained = state.overconstrained - id,
            constraints = state.constraints + (id -> currentConstraints),
            nodes = state.nodes + (requirement.id -> node))
        } else if (variants.size > 1) { //under-constrained
          None -> state.copy(
            resolved = state.resolved - id,
            resolvedVariants = state.resolvedVariants - id,
            underconstrained = state.underconstrained + id,
            overconstrained = state.overconstrained - id,
            constraints = state.constraints + (id -> currentConstraints),
            nodes = state.nodes + (requirement.id -> node))
        } else if (variants.size < 1) { //over-constrained
          None -> state.copy(
            resolved = state.resolved - id,
            resolvedVariants = state.resolvedVariants - id,
            underconstrained = state.underconstrained - id,
            overconstrained = state.overconstrained + id,
            constraints = state.constraints + (id -> currentConstraints),
            nodes = state.nodes + (requirement.id -> node))
        } else {
          throw new UnexpectedResolutionStateException("Unexpected number of variants: " + variants)
        }
    }
  }

  private[adept] def resolveNodes(requirements: Set[Requirement], state: State) = requirements.flatMap(requirement => state.nodes.get(requirement.id))

  private[adept] def resolveRequirements(requirements: Set[Requirement], visited: Set[Requirement], lastState: State): State = {
    val newRequirements = requirements.filter { requirement =>
      !visited(requirement) //remove requirements we have already visited
    }

    //TODO: evaluate whether breadth-first is normally faster than depth-first (used now) or find an heuristic that is good at _finding constraints_ that are _likely_ to be used: I believe this is what it is all about 
    newRequirements.foldLeft(lastState) { (state, requirement) => //collapse all requirements that can be found into one
      resolveVariant(requirement, state) match {
        case (Some(variant), resolvedState) =>
          val state =
            if (variant.requirements.isEmpty) resolvedState
            else resolveRequirements(variant.requirements, visited + requirement, resolvedState)

          val node = state.nodes(variant.id)

          state.copy(nodes = state.nodes +
            (variant.id -> node.copy(children =
              resolveNodes(variant.requirements, state))))
        case (None, unresolvedState) if !unresolvedState.isResolved =>
          unresolvedState
        case _ => throw new UnexpectedResolutionStateException("Could not find a variant for a resolved requirement: " + requirement + " state: " + state)
      }
    }
  }

  /**
   * Resolve the requirements and return the result.
   *
   * Is thread-safe.
   */
  def resolve(requirements: Set[Requirement]): ResolveResult = {
    val initState = new State(
      resolved = Set.empty,
      underconstrained = Set.empty,
      overconstrained = Set.empty,
      resolvedVariants = Map.empty,
      implicitVariants = Map.empty,
      constraints = Map.empty,
      nodes = Map.empty)

    val optimalUnderconstrainedStates = { //TODO: it would be better if we could find an _elegant_ to do this without mutating
      import adept.resolution.UnexpectedResolutionStateException;
      import collection.JavaConverters._
      java.util.Collections.newSetFromMap(
        new java.util.concurrent.ConcurrentHashMap[State, java.lang.Boolean]()).asScala
    }
    implicitResolve(requirements, initState, Set.empty, optimalUnderconstrainedStates) match {
      case Right(state) => new ResolvedResult(state, resolveNodes(requirements, state))
      case Left(failedState) =>
        if (failedState.isUnderconstrained) {
          new UnderconstrainedResult(failedState, resolveNodes(requirements, failedState), optimalUnderconstrainedStates.toSet)
        } else if (failedState.isOverconstrained) {
          new OverconstrainedResult(failedState, resolveNodes(requirements, failedState))
        } else {
          throw new UnexpectedResolutionStateException("Failed state was neither under-constrained nor over-constrained: " + failedState)
        }
    }
  }

  private def implicitResolve(requirements: Set[Requirement], currentState: State, previouslyUnderconstrained: Set[Id], optimalUnderconstrainedStates: collection.mutable.Set[State]): Either[State, State] = {
    val state = resolveRequirements(requirements, Set.empty, currentState)

    if (state.isUnderconstrained && skipImplicitResolve) {
      Left(state)
    } else if (state.isUnderconstrained && !skipImplicitResolve) {
      //under-constrained; perhaps there is a unique combination of variants where we still can resolve:

      val nonImplicitRequirements = requirements.filter { requirement =>
        !state.implicitVariants.isDefinedAt(requirement.id)
      }

      val ignoredIds = (state.implicitVariants.values.map { variant => //ignore id of implicit variants
        variant.id
      } ++ previouslyUnderconstrained).toSet //ignore ids which are under-constrained already

      //try out the different combinations till we find a unique combination that resolves
      val testedStatesCombinations = combinations(state.underconstrained, ignoredIds, state.constraints).map { combinations =>
        //TODO: .par to improve speed? Risk to have threading issues because of optimalUnderconstrainedStates which is mutable.
        combinations.map { combination =>
          val implicitVariants = combination.map { variant =>
            variant.id -> variant
          }.toMap

          val implicitState = state.copy(
            underconstrained = state.underconstrained -- combination.map(_.id), //we are no longer under-constrained on the implicitVariants
            implicitVariants = state.implicitVariants ++ implicitVariants)
          implicitResolve(nonImplicitRequirements, implicitState, ignoredIds ++ state.underconstrained, optimalUnderconstrainedStates) //ignore ids that are already under-constrained at this level
        }.collect {
          case Right(state) => state
        }.toList //TODO: is there a way to avoid this toList? We need to use this iterator to find *then* extract the state, which is why toList is there now.
      }
      testedStatesCombinations find (_.size > 0) match {
        case Some(resolvedState +: Nil) => Right(resolvedState) //found exactly one 
        case None => Left(state)
        case Some(states) =>
          var implicitsUsed = optimalUnderconstrainedStates.headOption.map(_.implicitVariants.size).getOrElse(-1)
          states.foreach { state =>
            if (state.isResolved && (state.implicitVariants.size < implicitsUsed || implicitsUsed == -1)) {
              //found a more optimal state than earlier, so remove the previous and add new ones
              implicitsUsed = state.implicitVariants.size
              optimalUnderconstrainedStates.clear()
              optimalUnderconstrainedStates += state
            } else if (state.isResolved && (state.implicitVariants.size == implicitsUsed || implicitsUsed == -1)) {
              optimalUnderconstrainedStates += state
            }
          }
          Left(state)
      }
    } else if (state.isOverconstrained) {
      Left(state)
    } else if (state.isResolved) {
      Right(state)
    } else {
      throw new UnexpectedResolutionStateException("State is neither resolved, underconstrained nor overconstrained: " + state)
    }

  }
}