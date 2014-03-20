package adept.ivy

import org.apache.ivy.core.resolve.IvyNode
import org.apache.ivy.core.module.descriptor.DependencyDescriptor
import org.eclipse.jgit.lib.ProgressMonitor
import adept.ext._
import adept.repository.GitRepository
import adept.repository.serialization._
import adept.repository.models._
import adept.resolution.models._
import java.io.File
import adept.logging.Logging

object IvyResolutionResults extends Logging {
  import IvyUtils._
  
  /** 
   *  Insert Ivy Import results (variants, resolution results, ...) into corresponding Adept repositories.
   *  Automatically orders variants according to versions.
   *  */
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

    val grouped = included.groupBy(_.repository) //grouping to avoid multiple parallel operations on same repo
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
          val commit = repository.commit("Ivy Import of " + variant.id)
          repository.add(VersionOrder.useDefaultVersionOrder(id, repository, commit))
          repository.commit("Ordered Ivy Import of " + variant.id)
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

          val currentResults = VersionOrder.createResolutionResults(baseDir, includedVersionInfo) ++
            Set(ResolutionResult(id, repository.name, repository.getHead, variantMetadata.hash))

          val resolutionResultsMetadata = ResolutionResultsMetadata(currentResults.toSeq)
          repository.add(resolutionResultsMetadata.write(id, variantMetadata.hash, repository))
          repository.commit("Resolution results of " + variant.id)
          currentResults
        }
        progress.update(1)
        completedResults
    }
    progress.endTask()

    progress.beginTask("GCing new Ivy repo(s)", grouped.size)
    grouped.par.foreach { //NOTICE .par TODO: same as above (IO vs CPU)
      case (name, _) =>
        val repository = new GitRepository(baseDir, name)
        repository.gc()
        progress.update(1)
    }
    progress.endTask()
    all
  }
}