package adept.repository

import adept.resolution.models._
import adept.repository.models._
import adept.repository.serialization.VariantMetadata
import java.io.File
import net.sf.ehcache.CacheManager
import adept.utils.Hasher
import net.sf.ehcache.Ehcache
import adept.repository.serialization.Order
import org.eclipse.jgit.lib.ProgressMonitor
import adept.repository.serialization.ResolutionResultsMetadata
import CacheHelpers._

object GitLoader {
  //TODO: caching
  //  private def hash(id: Id, constraints: Set[Constraint]): String = {
  //    val uniqueString = (id.value + constraints.toSeq.sorted.map(c => c.name + c.values.mkString(",").mkString(";")))
  //    Hasher.hash(uniqueString.getBytes)
  //  }
  //
  //  private def hash(id: Id, hash: VariantHash, repository: RepositoryName, commit: Commit): String = {
  //    val uniqueString = (id.value + hash.value + repository.value + commit.value)
  //    Hasher.hash(uniqueString.getBytes)
  //  }
  //
  //  private def hash(id: Id, repository: RepositoryName, commit: Commit): String = {
  //    val uniqueString = (id.value + repository.value + commit.value)
  //    Hasher.hash(uniqueString.getBytes)
  //  }
  //
  //  private def getCache(thisUniqueId: String, cacheManager: CacheManager): Ehcache = {
  //    val cacheName = thisUniqueId
  //    cacheManager.addCacheIfAbsent(cacheName)
  //    cacheManager.getEhcache(cacheName)
  //  }

  //  private def uniqueName(repositories: Set[(GitRepository, Commit)], requirements: Set[Requirement]) = {
  //    repositories.map {
  //      case (repository, commit) =>
  //        repository.name.value + commit.value
  //    }.toSeq.sorted.mkString("$") +
  //      requirements.map { requirement =>
  //        requirement.id.value + requirement.constraints.toSeq.sorted.mkString("-")
  //      }.toSeq.sorted.mkString(";")
  //  }

  def getCurrentResolutionResults(baseDir: File, requirements: Set[(RepositoryName, Requirement)], progress: ProgressMonitor, cacheManager: CacheManager): Set[ResolutionResult] = { //Set[(ResolutionResult, RepositoryLocations)] = { 
    val currentRequirements = requirements.map {
      case (name, requirement) =>
        val repository = new GitRepository(baseDir, name)
        (requirement, name, repository.getHead)
    }
    getResolutionResults(baseDir, currentRequirements, progress, cacheManager)
  }

  def getResolutionResults(baseDir: File, requirements: Set[(Requirement, RepositoryName, Commit)], progress: ProgressMonitor, cacheManager: CacheManager): Set[ResolutionResult] = {
    val latestRequirements = requirements.groupBy { case (requirement, name, _) => requirement.id -> name }.map {
      case ((id, name), values) =>
        val repository = new GitRepository(baseDir, name)
        val commits = values.map { case (_, _, commit: Commit) => commit }
        val constraints = values.flatMap { case (requirement: Requirement, _, _) => requirement.constraints }
        (id, constraints, repository, GitHelpers.lastestCommit(repository, commits))
    }

    //populate allVariants:
    var allVariants = Map.empty[Id, (Set[(Variant, GitRepository, Commit)], Set[Constraint])] //easier to read than folding
    for {
      (id, constraints, repository, commit) <- latestRequirements
      hash <- Order.activeVariants(id, repository, commit)
      variant <- VariantMetadata.read(id, hash, repository, commit)
    } { //<-- Notice (no yield)
      val (formerVariants: Set[(Variant, GitRepository, Commit)], formerConstraints: Set[Constraint]) = allVariants.getOrElse(id, (Set.empty[(Variant, GitRepository, Commit)], Set.empty[Constraint]))
      val allVariantInfo = formerVariants + ((variant, repository, commit))
      allVariants += id -> (allVariantInfo, (formerConstraints ++ constraints))
    }

    //find all transitive resolution results and add current
    val resolutionResults = for {
      (id, (vrs, constraints)) <- allVariants
      (variant, repository, commit) <- vrs
      matchingVariant <- AttributeConstraintFilter.filter(id, Set(variant), constraints)
      hash = VariantMetadata.fromVariant(matchingVariant).hash
      resolutionResult <- {
        val transitiveResolutionResults = ResolutionResultsMetadata.read(id, hash, repository, commit).map { metadata =>
          metadata.values
        }.getOrElse(Seq.empty[ResolutionResult])

        transitiveResolutionResults :+
          ResolutionResult(id, repository.name, commit, hash)
      }
    } yield {
      resolutionResult
      /*TODO:  {
        val locations: Set[RepositoryLocations] = RepositoryLocationsMetadata.read(id, resolutionResult.name, repository, commit).getOrElse{ Seq.empty[RepositoryLocatoins] }
        locations.toSet
      }*/
    }
    resolutionResults.toSet
  }

  def loadRepositories(repositories: Set[(ResolutionResult, RepositoryLocations)]) = {
    ???
  }
}

class GitLoader(repositories: Set[(GitRepository, Commit)], progress: ProgressMonitor, cacheManager: CacheManager) extends VariantsLoader {
  import GitLoader._
  private val thisUniqueId = Hasher.hash(repositories.map { case (repo, commit) => repo.name + "-" + commit.value }.toSeq.sorted.mkString("#").getBytes)

  private val cache: Ehcache = getCache(thisUniqueId, cacheManager)

  //  private lazy val byId = repositories.groupBy { case (r, _) => r.id }.map {
  //    case (id, repositoryInfos) => id -> repositoryInfos.flatMap {
  //      case (repositoryInfo, location) =>
  //        val repository = new GitRepository(baseDir, repositoryInfo.repository, progress)
  //        val commit = repositoryInfo.commit
  //        if (!repository.hasCommit(commit)) {
  //          location.uris.foreach { uri =>
  //            repository.fetchRemote(uri)
  //            repository.checkout(branch)
  //          }
  //        }
  //        val matches: Set[(VariantHash, GitRepository, Commit)] = Order.firstMatch(id, repositoryInfo.variants.hashes, repository, repositoryInfo.commit) match {
  //          case Some(FilePosition(variantSet, _)) => variantSet.hashes.map { variantHash =>
  //            (variantHash, repository, commit)
  //          }
  //          case None => Set.empty
  //        }
  //        matches
  //    }
  //  }

  private def locateAllIdentifiers(id: Id): Set[(VariantHash, GitRepository, Commit)] = {
    //    byId(id)
    ???
  }

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    val cacheKey = "v" + hash(id, constraints)
    usingCache(cacheKey, cache) {
      val allVariants: Set[Variant] = {
        locateAllIdentifiers(id).flatMap {
          case (hash, repository, commit) =>
            VariantMetadata.read(id, hash, repository, commit)
        }
      }
      AttributeConstraintFilter.filter(id, allVariants, constraints)
    }
  }

}