package adept.ivy

import org.apache.ivy.core.resolve.IvyNode
import org.apache.ivy.core.module.descriptor.DependencyDescriptor
import org.eclipse.jgit.lib.ProgressMonitor
import adept.ext._
import adept.repository.GitRepository
import adept.repository.serialization._
import adept.repository.models._
import adept.resolution.models._
import adept.artifact.ArtifactCache
import adept.artifact.models._
import java.io.File
import adept.logging.Logging

object IvyImportResultInserter extends Logging {
  import IvyUtils._

  /**
   * Defaults to ranking per version
   *
   *  @returns files that must be added and files that removed
   */
  protected def useDefaultVersionRanking(id: Id, repository: GitRepository, commit: Commit): (Set[File], Set[File]) = {
    VersionRank.useSemanticVersionRanking(id, repository, commit, excludes = Set(".*".r)) //use versions only
  }

  def getExistingResolutionResult(baseDir: File, ivyImportResult: IvyImportResult): Option[Set[ResolutionResult]] = {
    val repository = new GitRepository(baseDir, ivyImportResult.repository)
    if (repository.exists) {
      val id = ivyImportResult.variant.id
      val commit = repository.getHead
      val hash = VariantMetadata.fromVariant(ivyImportResult.variant).hash
      for {
        _ <- VariantMetadata.read(id, hash, repository, commit, checkHash = true)
        resolutionResultsMetadata <- ResolutionResultsMetadata.read(id, hash, repository, commit)
      } yield {
        resolutionResultsMetadata.values.toSet
      }
    } else {
      None //no repository => result does not exist
    }
  }

  /**
   *  Insert Ivy Import results (variants, resolution results, ...) into corresponding Adept repositories.
   *  Automatically ranks variants according to useDefaultVersionRanking.
   */
  def insertAsResolutionResults(baseDir: File, results: Set[IvyImportResult], progress: ProgressMonitor): Set[ResolutionResult] = {
    progress.beginTask("Applying exclusion(s)", results.size * 2)
    val included = results.flatMap { result =>
      var requirementModifications = Map.empty[Id, Set[Variant]]
      var currentExcluded = false

      for {
        otherResult <- results
        ((variantId, requirementId), excludeRules) <- result.excludeRules
        excludeRule <- excludeRules
        if (matchesExcludeRule(excludeRule, otherResult.variant))
      } { //<-- NOTICE
        if (variantId == result.variant.id) {
          logger.debug("on variant: " + variantId + " add exclusion for " + requirementId + ":" + excludeRules)
          val formerlyExcluded = requirementModifications.getOrElse(requirementId, Set.empty[Variant])
          requirementModifications += requirementId -> (formerlyExcluded + otherResult.variant) //MUTATE!
        }
        if (requirementId == result.variant.id) {
          logger.debug("on result: " + result.variant.id + " will be excluded because of: " + excludeRules)
          currentExcluded = true //MUTATE!
        }
      }
      val fixedResult = if (requirementModifications.nonEmpty) {
        val fixedRequirements = result.variant.requirements.map { requirement =>
          requirementModifications.get(requirement.id).map { excludedVariants =>
            logger.debug("Excluding: " + excludedVariants.map(_.id) + " on " + requirement.id + " in " + result.variant.id)
            requirement.copy(
              exclusions = requirement.exclusions ++ excludedVariants.map(_.id))
          }.getOrElse(requirement)
        }
        val fixedVariant = result.variant.copy(requirements = fixedRequirements)
        result.copy(variant = fixedVariant)
      } else {
        result
      }

      if (currentExcluded) {
        None
      } else {
        Some(fixedResult)
      }
    }
    progress.endTask()

    val previousResolutionResults = included.flatMap{ result=> 
      getExistingResolutionResult(baseDir, result).map(result -> _)
    }.toMap
    
    val (existingResults, newImports) = included.partition { result =>
      previousResolutionResults.contains(result)  
    }

    val grouped = newImports.groupBy(_.repository) //grouping to avoid multiple parallel operations on same repo
    progress.beginTask("Writing Ivy results to repo(s)", grouped.size)
    grouped.par.foreach { //NOTICE .par TODO: replace with something more optimized for IO not for CPU
      case (_, results) =>
        results.foreach { result =>
          val variant = result.variant
          val id = variant.id
          val repository = new GitRepository(baseDir, result.repository)
          if (!repository.exists) repository.init()
          val variantMetadata = VariantMetadata.fromVariant(variant)
          repository.add(variantMetadata.write(id, repository))
          result.artifacts.foreach { artifact =>
            repository.add(ArtifactMetadata.fromArtifact(artifact).write(artifact.hash, repository))
          }
          val commit = repository.commit("Ivy Import of " + variant.id) //TODO: We could remove this commit, and I suspect things will go a bit faster
          val (addFiles, rmFiles) = useDefaultVersionRanking(id, repository, commit)
          repository.add(addFiles)
          repository.rm(rmFiles)
          repository.commit("Ranked Ivy Import of " + variant.id)
        }
        progress.update(1)
    }
    progress.endTask()
    progress.beginTask("Converting Ivy version in repo(s)", grouped.size)
    val all = Set() ++ grouped.par.flatMap { //NOTICE .par TODO: same as above (IO vs CPU)
      case (_, results) =>
        val completedResults = results.flatMap { result =>
          val variant = result.variant
          val id = variant.id

          val repository = new GitRepository(baseDir, result.repository)
          if (!repository.exists) repository.init()
          val variantMetadata = VariantMetadata.fromVariant(variant)

          val includedVersionInfo = result.versionInfo

          val currentResults = VersionRank.createResolutionResults(baseDir, includedVersionInfo) ++
            Set(ResolutionResult(id, repository.name, repository.getHead, variantMetadata.hash))

          val resolutionResultsMetadata = ResolutionResultsMetadata(currentResults.toSeq)
          repository.add(resolutionResultsMetadata.write(id, variantMetadata.hash, repository))
          repository.commit("Resolution results of " + variant.id)
          currentResults
        }
        progress.update(1)
        completedResults
    } ++ existingResults.flatMap(previousResolutionResults)
    progress.endTask()

    progress.beginTask("GCing new Ivy repo(s)", grouped.size)
    grouped.par.foreach { //NOTICE .par TODO: same as above (IO vs CPU)
      case (name, _) =>
        val repository = new GitRepository(baseDir, name)
        repository.gc()
        progress.update(1)
    }
    progress.endTask()

    progress.beginTask("Copying files to Adept cache", grouped.size)
    grouped.par.foreach { //NOTICE .par TODO: same as above (IO vs CPU)
      case (_, results) =>
        for {
          result <- results
          artifact <- result.variant.artifacts
          (expectedHash, file) <- result.localFiles
          if artifact.hash == expectedHash
        } { //<- NOTICE
          ArtifactCache.cache(baseDir, file, expectedHash, artifact.filename.getOrElse(file.getName)) match {
            case Left(error) => logger.error("While copying artifacts for: " + result.variant.id + " got: " + error + ". Variant: " + result.variant + " (src file: " + file.getAbsolutePath + ")")
            case _ => //we do not check if we got the right amount of artifacts, because it can always be downloaded later. Caching is done as a convenience if we are going to use the Ivy import directly. 
          }
        }
        progress.update(1)
    }
    progress.endTask()
    all
  }
}