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
  var resolved: Set[String] = Set.empty,
  var overconstrained: Set[String] = Set.empty,
  var underconstrained: Set[String] = Set.empty,
  var forcedVariants: Map[String, Variant] = Map.empty,
  var resolvedVariants: Map[String, Variant] = Map.empty,

  var constraints: Map[String, Set[Constraint]] = Map.empty) {
  override def toString = {
    "resolved: " + resolved + "\n" +
      "over-constrained: " + overconstrained + "\n" +
      "under-constrained: " + underconstrained + "\n" +
      "resolved-variants: " + resolvedVariants + "\n" +
      "forced-variants: " + forcedVariants + "\n" +
      "constraints: " + constraints + "\n"
  }

  def copy() = {
    new State(
      resolved = resolved,
      underconstrained = underconstrained,
      overconstrained = overconstrained,
      resolvedVariants = (resolvedVariants.keys zip resolvedVariants.values).toMap, //FIXME: hack to make a new copy of values
      forcedVariants = (forcedVariants.keys zip forcedVariants.values).toMap, //FIXME: hack to make a new copy of values
      constraints = (constraints.keys zip constraints.values).toMap) //FIXME: hack to make a new copy of values
  }

  def copy(forcedVariants: Map[String, Variant]) = {
    new State(
      resolved = resolved,
      underconstrained = underconstrained,
      overconstrained = overconstrained,
      forcedVariants = forcedVariants,
      constraints = constraints)
  }
}

/* TODO: not sure if vars give us anything here either */
case class Node(val id: String, var children: Set[Node])


class Resolver(variantsLoader: VariantsLoaderEngine) extends VariantsLoaderLogic {

  private[adept] def getConstraints(dependency: Dependency, state: State) = {
    dependency.constraints ++ state.constraints.get(dependency.id).getOrElse(Set.empty)
  }

  private[adept] def getVariants(id: String, state: State) = {
    state.forcedVariants.get(id) match {
      case Some(variant) => Set(variant)
      case None => variantsLoader.get(id, state.constraints(id))
    }
  }

  private[adept] def resolveVariants(id: String, variants: Set[Variant], state: State) = {
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

  private[adept] def resolveDependencies(dependencies: Seq[Dependency], state: State): Unit = {
    dependencies.foreach { dependency: Dependency =>
      val id = dependency.id
      val constraints = getConstraints(dependency, state)
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

  def resolve(dependencies: Seq[Dependency], initState: Option[State] = None): Either[State, State] = { //TODO: Seq should be Set
    def resolve(depedencies: Seq[Dependency], state: State, checkedCombinations: Set[Set[Variant]]): Option[State] = {
      resolveDependencies(dependencies, state)

      if (state.overconstrained.size == 0 && state.underconstrained.size > 0) {
        //under-constrained, but not over-constrained, so try to find the first set of variants that can resolve this:

        val allVariantCombinations = variantCombinations(state.underconstrained, state)
        //amount of combinations required to iterate over: state.underconstrained.size * allVariantCombinations
        //TODO: limit to a max number of paths to try?
        val combinationSize = state.underconstrained.size

        val combinationSets = for { //the ordered sequence of each size of combinations to try, starting with the least amount of variants to constrain
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

        //view makes it possible to map then find for each combination, without having to continue if one successful combination is found
        val resolvedStates = combinationSets.view map { combinations => //find first set of combinations which...
          val states = combinations.toList.flatMap { combination => //...has only one combination that resolves
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
              
              if (alreadyForced) None 
              else {
                val forcedState = state.copy(forcedVariants = forcedVariants)
                resolve(dependencies, forcedState, checkedCombinations + combination)
              }
            }
          }
          states
        } find { resolvedStates =>
          resolvedStates.size == 1
        }

        resolvedStates match {
          case Some(found :: Nil) => Some(found) //we found one resolved state for all current combinations
          case _ => None //none or more than one resolved states so we failed
        }
      } else if (state.underconstrained.size == 0 && state.overconstrained.size == 0) {
        Some(state)
      } else None
    }

    val currentState = initState.getOrElse(new State())
    resolve(dependencies, currentState, Set.empty) match {
      case Some(state) => Right(state) //found a state that resolves
      case None => Left(currentState) //found no path to resolution, so return first graph for debug
    }
  }
}

