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
import adept.logging.Logging

object GitLoader extends Logging {
  private def hash(id: Id, constraints: Set[Constraint], uniqueId: String): String = {
    val uniqueString = (id.value + constraints.toSeq.sorted.map(c => c.name + c.values.mkString(",").mkString(";")) + uniqueId)
    Hasher.hash(uniqueString.getBytes)
  }

  private val CacheName = "adept-cache"

  private def getCache(cacheManager: CacheManager): Ehcache = {
    cacheManager.addCacheIfAbsent(CacheName)
    cacheManager.getEhcache(CacheName)
  }

  private def hash(requirements: Set[(Requirement, RepositoryName, Commit)]): String = {
    val uniqueString = requirements.map {
      case (requirement, repository, commit) =>
        hash(requirement.id, requirement.constraints, repository.value + commit.value)
    }.toSeq.sorted.mkString("$")
    Hasher.hash(uniqueString.getBytes)
  }

  def getLatestResolutionResults(baseDir: File, requirements: Set[(RepositoryName, Requirement)], progress: ProgressMonitor, cacheManager: CacheManager): Set[(ResolutionResult, RepositoryLocations)] = {
    val currentRequirements = requirements.map {
      case (name, requirement) =>
        val repository = new GitRepository(baseDir, name)
        (requirement, name, repository.getHead)
    }
    getResolutionResults(baseDir, currentRequirements, progress, cacheManager)
  }

  def getResolutionResults(baseDir: File, requirements: Set[(Requirement, RepositoryName, Commit)], progress: ProgressMonitor, cacheManager: CacheManager): Set[(ResolutionResult, RepositoryLocations)] = {
    usingCache(key = hash(requirements), getCache(cacheManager)) {
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

      //find all transitive resolution results and add the ones for each requirement
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
        /*TODO:
        val locations: Set[RepositoryLocations] = RepositoryLocationsMetadata.read(id, resolutionResult.name, repository, commit).getOrElse{ Seq.empty[RepositoryLocations] }
        */
        resolutionResult -> fakeLocation(resolutionResult.repository.value)
      }

    }
  }

  def fakeLocation(name: String) = { //TODO: replace this with something REAL
    logger.warn("Using fake location for: " + name)
    RepositoryLocations(Set("https://github.com/adept-test-repo1/" + name))
  }

  def fetchLocations(repository: GitRepository, repositoryLocations: RepositoryLocations, progress: ProgressMonitor, passphrase: Option[String]) = {
    repositoryLocations.uris.map { uri =>
      repository.fetchRemote(uri, passphrase, progress)
      repository.checkout(repository.getHead.value) //TODO: this is not needed - but it is nice for the user (because you can actually see some files) but I am not sure?
    }
  }

  def loadRepositories(baseDir: File, repositories: Set[(ResolutionResult, RepositoryLocations)], progress: ProgressMonitor, passphrase: Option[String]): Set[Commit] = {
    Set() ++ repositories.par.map { //<-- NOTICE .par
      case (resolutionResult, repositoryLocations) =>
        val repository = new GitRepository(baseDir, resolutionResult.repository)
        if (!repository.exists) {
          repository.init()
        }

        if (!repository.hasCommit(resolutionResult.commit)) {
          fetchLocations(repository, repositoryLocations, progress, passphrase)
          if (!repository.hasCommit(resolutionResult.commit)) {
            throw new Exception("Could not fetch commit: " + resolutionResult.commit.value + " for " + repository.dir.getAbsolutePath)
          }
        }
        resolutionResult.commit
    }
  }
}

class GitLoader(repositories: Set[(GitRepository, Commit)], progress: ProgressMonitor, cacheManager: CacheManager) extends VariantsLoader {
  import GitLoader._
  private val thisUniqueId = Hasher.hash(repositories.map { case (repo, commit) => repo.name + "-" + commit.value }.toSeq.sorted.mkString("#").getBytes)

  private val cache: Ehcache = getCache(cacheManager)

    private lazy val byId = repositories.groupBy { case (repository, commit) => r.id }.map {
      case (id, repositoryInfos) => id -> repositoryInfos.flatMap {
        case (repositoryInfo, location) =>
          val repository = new GitRepository(baseDir, repositoryInfo.repository, progress)
          val commit = repositoryInfo.commit
          if (!repository.hasCommit(commit)) {
            location.uris.foreach { uri =>
              repository.fetchRemote(uri)
              repository.checkout(branch)
            }
          }
          val matches: Set[(VariantHash, GitRepository, Commit)] = Order.firstMatch(id, repositoryInfo.variants.hashes, repository, repositoryInfo.commit) match {
            case Some(FilePosition(variantSet, _)) => variantSet.hashes.map { variantHash =>
              (variantHash, repository, commit)
            }
            case None => Set.empty
          }
          matches
      }
    }

  private def locateAllIdentifiers(id: Id): Set[(VariantHash, GitRepository, Commit)] = {
    //    byId(id)
    ???
  }

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    val cacheKey = "v" + hash(id, constraints, thisUniqueId)
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