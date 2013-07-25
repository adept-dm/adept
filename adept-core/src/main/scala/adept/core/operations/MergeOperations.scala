package adept.core.operations

import adept.core.models._
import adept.core.Adept
import adept.utils.Logging

private[core] object MergeOperations extends Logging {

  /**
   * Merging artifacts consist primarily of merging the locations of artifacts that are similar, i.e. same hash and types
   * Because we merge locations, we also merge configurations. I am not sure if this is the right thing to do.
   * The reason I think it is right now, is that for all configurations that you wanted a particular artifact, you want it to show up when you merge
   * TODO: make this doc a bit slimmer and easier to understand
   */
  private[operations] def mergeArtifacts(artifacts1: Set[Artifact], artifacts2: Set[Artifact]): Set[Artifact] = {
    import collection.{ Set => _, _ }
    val (merged, added) = (for {
      artifact1 <- artifacts1
      artifact2 <- artifacts2 if (artifact1.hash == artifact2.hash && artifact1.artifactType == artifact2.artifactType)
    } yield {
      val hash = artifact1.hash //we are filtering on hash
      val artifactType = artifact1.artifactType //we are filtering on artifactType

      val configurations = artifact1.configurations ++ artifact2.configurations
      val locations = artifact1.locations ++ artifact2.locations

      Artifact(hash, artifactType, configurations, locations) -> (hash, artifactType)
    }).unzip

    val rest = for {
      artifact <- (artifacts1 ++ artifacts2) if (!added.contains(artifact.hash -> artifact.artifactType))
    } yield {
      artifact
    }

    rest.toSet ++ merged
  }

  /**
   * Merging modules consist primarily of merging the locations of artifacts that are similar, i.e. same hash and types
   * Because we merge artifacts, we also merge configurations.
   * @see mergeArtifacts
   *
   * TODO: make this doc a bit slimmer and easier to understand
   */
  private[operations] def mergeModules(module1: Module, module2: Module): Either[Set[Module], Module] = {
    if (module1.coordinates != module2.coordinates) {
      throw new Exception("cannot merge modules: " + module1 + " and " + module2 + " because they have different coordinates")
    } else if (module1.uniqueId != module2.uniqueId) {
      throw new Exception("cannot merge modules: " + module1 + " and " + module2 + " because they have different unique ids")
    } else if (module1.dependencies != module2.dependencies) {
      logger.error("cannot merge modules: " + module1 + " and " + module2 + " because they have different dependencies")
      Left(Set(module1, module2))
    } else if (module1.overrides != module2.overrides) {
      logger.error("cannot merge modules: " + module1 + " and " + module2 + " because they have different overrides")
      Left(Set(module1, module2))
    } else if (module1.universes != module2.universes) {
      logger.error("cannot merge modules: " + module1 + " and " + module2 + " because they have different universes")
      Left(Set(module1, module2))
    } else {
      val coordinates = module1.coordinates
      val dependencies = module1.dependencies
      val overrides = module1.overrides
      val universes = module1.universes
      val uniqueId = module1.uniqueId
      
      val artifacts = mergeArtifacts(module1.artifacts, module2.artifacts)
      val configurations = module1.configurations ++ module2.configurations
      val attributes = module1.attributes ++ module2.attributes

      Right(Module(coordinates, uniqueId, universes, artifacts, configurations, attributes, dependencies, overrides))
    }

  }

  def mergeFindModules(repositories: Set[Adept]): Adept.FindModule = {
    val findModuleFun = (coords: Coordinates, uniqueId: Option[UniqueId], universes: Set[Universe]) => {
      repositories.par.foldLeft(Right(None): Either[Set[Module], Option[Module]]) { (result, adept) => //TODO: check .par for speed and put in IO Execution context?
        val current = adept.findModule(coords, uniqueId, universes)
        (result, current) match {
          case (Right(Some(previousModule)), Right(Some(currentModule))) => {
            mergeModules(previousModule, currentModule) match {
              case Right(module) => Right(Some(module))
              case Left(errorModules) => Left(errorModules)
            }
          }
          case (Right(None), Right(Some(currentModule))) => Right(Some(currentModule))
          case (Right(None), Right(None)) => Right(None)
          case (Right(Some(previousModule)), Right(None)) => Right(Some(previousModule))
          case (Right(maybeModule), Left(newErrors)) => Left(newErrors ++ maybeModule)
          case (Left(errorModules), Right(maybeModule)) => Left(errorModules ++ maybeModule)
          case (Left(errorModules), Left(newErrors)) => Left(errorModules ++ newErrors)
        }

      }
    }
    findModuleFun
  }

}