package adept.ext

import adept.core.models._
import adept.ext.AttributeDefaults._
import adept.core.resolution._

case class Resolution

object VersionConflictResolver {

  /**
   * Finds the highest compatible versions and returns:
   * The new result (can still be over-constrained, but not on versions);
   * The new variants which were created
   *
   * TODO: sniff sniff. is there a code smell here? consider rewriting, overrides and this part - just seems very complicated to me
   */
  def resolveHighestConflicts(result: ResolveResult, dependencies: Set[Dependency], loaderEngine: VariantsLoaderEngine): Either[(ResolveResult, Set[(Id, Set[Constraint])]), (ResolveResult, Set[Variant])] = { //TODO: could we simplify this type signature with some classes_
    val initVariants = result.state.resolvedVariants ++ result.state.implicitVariants

    def indexConstraints(ids: Set[Id], constraints: Map[Id, Set[Constraint]]) = ids.map(id => id -> constraints(id))

    def resolveHighestVersions(result: OverconstrainedResult,
      conflictingVersions: Set[(Id, Set[Constraint])], visitedConflicts: Set[Set[(Id, Set[Constraint])]]): Either[(ResolveResult, Set[(Id, Set[Constraint])]), (ResolveResult, Set[Variant])] = {

      val overconstrained = indexConstraints(result.state.overconstrained, result.state.constraints)

      val previousVariants = result.state.resolvedVariants ++ result.state.implicitVariants

      val newVariants = overconstrained.map {
        case (id, constraints) =>
          val (lastVersion, (query, replacementAttribute)) = queryAttributeReplacement(id, constraints, dependencies)
          val resolvedVariants = loaderEngine.get(id, constraints.filter(_.name != VersionAttribute) + Constraint(VersionAttribute, Set(lastVersion.value)))

          if (resolvedVariants.size == 0) {
            Left(id -> constraints)
          } else {
            //find the "clean" variant without overrides:
            val candidates = resolvedVariants.filter(_.attributes.find(_.name == OverridesAttribute).isEmpty)
            if (candidates.size == 1) {
              Right(id -> candidates.head)
            } else {
              Left(id -> constraints)
            }
          }
      }

      val errors = newVariants.collect {
        case Left(value) => value
      }

      if (errors.nonEmpty) {
        Left(result -> errors)
      } else {
        val requiredVariants = previousVariants ++ newVariants.map(_.right.get)

        val knownVariants =
          conflictingVersions.foldLeft(Map.empty[Id, Variant]) { //applying overrides:
            case (current, (id, constraints)) =>
              val (lastVersion, (query, replacementAttribute)) = queryAttributeReplacement(id, constraints, dependencies)
              val replaceResult = Extensions.overrides(dependencies, result.graph, requiredVariants ++ current, query, replacementAttribute)

              current ++ replaceResult.includedVariants
          }

        val allNewVariants = knownVariants.map { case (_, variant) => variant }

        def onlyNewVariants = allNewVariants.filter(newVariant => initVariants.get(newVariant.id) != Some(newVariant)).toSet

        val resolver = new Resolver(new DefinedVariants(allNewVariants.toSeq, loaderEngine.logic))
        val newResult = resolver.resolve(dependencies) //we are using the same dependencies because we can: Adept should be able to resolve if it is possible out for us
        newResult match {
          case overconstrainedResult: OverconstrainedResult =>
            val currentConflictingVersions = findConflicts(indexConstraints(result.state.overconstrained, result.state.constraints))

            //check if we have tried to resolve this conflict before, if not try to resolve if versions conflicts are detected
            if (!visitedConflicts(currentConflictingVersions) && currentConflictingVersions.nonEmpty)
              resolveHighestVersions(overconstrainedResult, currentConflictingVersions, visitedConflicts + currentConflictingVersions)
            else Right(overconstrainedResult -> onlyNewVariants)
          case result => Right(result -> onlyNewVariants)
        }
      }
    }
    result match {
      case result: OverconstrainedResult =>
        resolveHighestVersions(result, findConflicts(indexConstraints(result.state.overconstrained, result.state.constraints)), Set.empty)
      case _ => Right(result -> Set.empty)
    }
  }

  /**
   * Find the best version (as Maven defines it, see next line) to use and constructs query and replacements attributes.
   * Best version == the highest one specified in the dependencies or the highest transitive
   */
  private def queryAttributeReplacement(id: Id, constraints: Set[Constraint], dependencies: Set[Dependency]) = {
    val matchingDependencies = dependencies.filter(dependency => dependency.id == id)

    val versions =
      if (matchingDependencies.size > 1) {
        val constraints = matchingDependencies.flatMap(_.constraints)
        constraints.filter(_.name == VersionAttribute).flatMap(_.values).map(Version.apply)
      } else {
        constraints.filter(_.name == VersionAttribute).flatMap(_.values).map(Version.apply)
      }

    val lastVersion = versions.toSeq.sorted.last
    val newVersion = Attribute(VersionAttribute, Set(lastVersion.value)) //the lastVersion is the highest
    lastVersion -> (Query(id), Map(id -> Set(newVersion)))
  }

  private def findConflicts(idConstraints: Set[(Id, Set[Constraint])]) = idConstraints.filter {
    case (id, constraints) =>
      val versions = constraints.filter(_.name == VersionAttribute)
      // more than 1 versions means conflict
      versions.size > 1
  }

}