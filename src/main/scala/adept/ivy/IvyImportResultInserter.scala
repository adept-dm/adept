package adept.ivy

import org.apache.ivy.core.resolve.IvyNode
import org.apache.ivy.core.module.descriptor.DependencyDescriptor
import org.eclipse.jgit.lib.ProgressMonitor
import adept.ext._
import adept.repository.GitRepository
import adept.repository.metadata._
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
  protected def useDefaultVersionRanking(id: Id, variant: VariantHash, repository: GitRepository, commit: Commit): (Set[File], Set[File]) = {
    val rankId = RankingMetadata.defaultRankId(id, repository)
    val hashes = RankingMetadata.read(id, rankId, repository, commit).toSeq.flatMap(_.variants)
    val variants = (hashes :+ variant).flatMap{ hash =>
      VariantMetadata.read(id, hash, repository, commit).map(_.toVariant(id))
    }
    val sortedVariants = VersionRank.getSortedByVersions(variants)
    Set(RankingMetadata(sortedVariants).write(id, rankId, repository)) -> Set.empty[File]
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
      progress.update(2)

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

    progress.beginTask("Checking for new imports", included.size * 2)
    val previousResolutionResults = included.flatMap { result =>
      progress.update(1)
      getExistingResolutionResult(baseDir, result).map(result -> _)
    }.toMap

    val (existingResults, newImports) = included.partition { result =>
      progress.update(1)
      previousResolutionResults.contains(result)
    }
    progress.endTask()

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
          val (addFiles, rmFiles) = useDefaultVersionRanking(id, VariantMetadata.fromVariant(variant).hash, repository, commit)
          repository.add(addFiles)
          repository.rm(rmFiles)
          repository.commit("Ranked Ivy Import of " + variant.id)
        }
        progress.update(1)
    }
    progress.endTask()
    progress.beginTask("Converting Ivy version in repo(s)", grouped.size)
    val all = Set() ++ grouped.par.flatMap { //NOTICE .par TODO: same as above (IO vs CPU)
      case (name, results) =>
        val repository = new GitRepository(baseDir, name)
        if (!repository.exists) repository.init()
        val completedResults = results.flatMap { result =>
          val variant = result.variant
          val id = variant.id

          val variantMetadata = VariantMetadata.fromVariant(variant)

          val includedVersionInfo = result.versionInfo

          try {
            val currentResults = VersionRank.createResolutionResults(baseDir, includedVersionInfo) ++
              Set(ResolutionResult(id, repository.name, repository.getHead, variantMetadata.hash))

            val resolutionResultsMetadata = ResolutionResultsMetadata(currentResults.toSeq)
            repository.add(resolutionResultsMetadata.write(id, variantMetadata.hash, repository))
            currentResults
          } catch {
            case RepositoryNotFoundException(targetName, targetId, targetVersion) =>
              logger.warn("In: " + result.variant.id + " tried to find " + targetId + " version: " + targetVersion + " in repository: " + targetName + " but the repository was not there. Assuming it is an UNAPPLIED override (i.e. an override of a module which the source module does not actually depend on) so ignoring...")
              Set.empty[ResolutionResult]
            case VersionNotFoundException(targetName, targetId, targetVersion) =>
              logger.warn("In: " + result.variant.id + " tried to find " + targetId + " version: " + targetVersion + " in repository: " + targetName + " but that version was not there. Assuming it is an UNAPPLIED override (i.e. an override of a module which the source module does not actually depend on) so ignoring...")
              Set.empty[ResolutionResult]
          }
        }
        repository.commit("Updated resolution results")
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
          if (file.isFile) //sometimes Ivy removes the files for an very unknown reason. We do not care though, we can always download it again....
            ArtifactCache.cache(baseDir, file, expectedHash, artifact.filename.getOrElse(file.getName))
        }
        progress.update(1)
    }
    progress.endTask()
    all
  }
}