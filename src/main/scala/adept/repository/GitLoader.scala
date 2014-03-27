package adept.repository

import adept.resolution.models._
import adept.repository.models._
import adept.repository.serialization.VariantMetadata
import java.io.File
import net.sf.ehcache.CacheManager
import adept.utils.Hasher
import net.sf.ehcache.Ehcache
import org.eclipse.jgit.lib.ProgressMonitor
import adept.repository.serialization.ResolutionResultsMetadata
import adept.logging.Logging
import adept.repository.serialization.RepositoryLocationsMetadata
import adept.repository.serialization.RankingMetadata

object GitLoader extends Logging {

  private def hash(id: Id, constraints: Set[Constraint], uniqueId: String): String = {
    val uniqueString = "idc" + (id.value + constraints.toSeq.sorted.map(c => c.name + c.values.mkString(",").mkString(";")) + uniqueId)
    Hasher.hash(uniqueString.getBytes)
  }

  private val CacheName = "adept-cache"

  private def getCache(cacheManager: CacheManager): Ehcache = {
    cacheManager.addCacheIfAbsent(CacheName)
    cacheManager.getEhcache(CacheName)
  }

  private def hash(requirements: Set[(RepositoryName, Requirement, Commit)]): String = {
    val uniqueString = "rrc" + requirements.map {
      case (repository, requirement, commit) =>
        hash(requirement.id, requirement.constraints, repository.value + commit.value)
    }.toSeq.sorted.mkString("$")
    Hasher.hash(uniqueString.getBytes)
  }

  def getLatestResolutionResults(baseDir: File, requirements: Set[(RepositoryName, Requirement)], progress: ProgressMonitor, cacheManager: CacheManager): Set[(ResolutionResult, Option[RepositoryLocations])] = {
    val currentRequirements = requirements.map {
      case (name, requirement) =>
        val repository = new GitRepository(baseDir, name)
        (name, requirement, repository.getHead)
    }
    getResolutionResults(baseDir, currentRequirements, progress, cacheManager)
  }

  import adept.utils.CacheHelpers.usingCache

  def getResolutionResults(baseDir: File, requirements: Set[(RepositoryName, Requirement, Commit)], progress: ProgressMonitor, cacheManager: CacheManager): Set[(ResolutionResult, Option[RepositoryLocations])] = {
    usingCache(key = "getResolutionResults" + hash(requirements), getCache(cacheManager)) {
      val latestRequirements = requirements.groupBy { case (name, requirement, _) => requirement.id -> name }.map {
        case ((id, name), values) =>
          val repository = new GitRepository(baseDir, name)
          val commits = values.map { case (_, _, commit: Commit) => commit }
          val constraints = values.flatMap { case (_, requirement: Requirement, _) => requirement.constraints }
          (id, constraints, repository, GitHelpers.lastestCommit(repository, commits).getOrElse(throw new Exception("Could not find a latest commit between (is empty or cannot compare?): " + commits + " in " + repository.dir.getAbsolutePath)))
      }

      //populate allVariants:
      var allVariants = Map.empty[Id, (Set[(Variant, GitRepository, Commit)], Set[Constraint])] //easier to read than folding
      for {
        (id, constraints, repository, commit) <- latestRequirements
        hash <- {
          val rankIds = RankingMetadata.listRankIds(id, repository, commit)
          val rankings = rankIds.flatMap { rankId =>
            RankingMetadata.read(id, rankId, repository, commit).map(_.toRanking(id, rankId))
          }
          val foundHashes = RankLogic.activeVariants(rankings)
          if (foundHashes.isEmpty) logger.warn("Could not find any ranking matching: " + id + " in " + repository.dir.getAbsolutePath + " for " + commit)
          foundHashes
        }
        metadata <- {
          val maybeMetadata = VariantMetadata.read(id, hash, repository, commit)
          if (!maybeMetadata.isDefined) logger.warn("Could not find variant metadata: " + hash + " for id: " + id + " in dir " + repository.dir.getAbsolutePath + " for " + commit)
          maybeMetadata
        }
      } { //<-- Notice (no yield)
        val variant = metadata.toVariant(id)
        val (formerVariants: Set[(Variant, GitRepository, Commit)], formerConstraints: Set[Constraint]) = allVariants.getOrElse(id, (Set.empty[(Variant, GitRepository, Commit)], Set.empty[Constraint]))
        val allVariantInfo = formerVariants + ((variant, repository, commit))
        allVariants += id -> (allVariantInfo, (formerConstraints ++ constraints))
      }

      //find all transitive resolution results and add the ones for each requirement
      val resolutionResults = for {
        (id, (vrs, constraints)) <- allVariants.toSet
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
        val locations =
          RepositoryLocationsMetadata.read(resolutionResult.repository, repository, commit).map(_.toRepositoryLocations(resolutionResult.repository))

        resolutionResult -> locations
      }
      resolutionResults
    }
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

class GitLoader(baseDir: File, private[adept] val results: Set[ResolutionResult], progress: ProgressMonitor, cacheManager: CacheManager) extends VariantsLoader {
  import GitLoader._
  import adept.utils.CacheHelpers.usingCache
  
  private val thisUniqueId = Hasher.hash(results.map { resolution => resolution.id.value + "-" + resolution.repository.value + "-" + resolution.variant.value + "-" + resolution.commit.value }.toSeq.sorted.mkString("#").getBytes)

  private val cache: Ehcache = getCache(cacheManager)

  private lazy val cachedById = usingCache("byId" + thisUniqueId, cache) { //lazy this might take a while
    results.groupBy(_.id).map {
      case (id, results) =>
        val latestCommitsOnly = results.groupBy(_.repository).flatMap {
          case (repositoryName, results) =>
            val repository = new GitRepository(baseDir, repositoryName)
            //use only latest commit:
            val maybeLatestCommit = GitHelpers.lastestCommit(repository, results.map(_.commit))
            maybeLatestCommit.toSet.flatMap { commit: Commit =>
              val variants = results.map(_.variant)
              //use only the very best variants for any given commit:
              val chosenVariants = {
                val rankIds = RankingMetadata.listRankIds(id, repository, commit)
                val rankings = rankIds.flatMap { rankId =>
                  RankingMetadata.read(id, rankId, repository, commit).map(_.toRanking(id, rankId))
                }
                RankLogic.chosenVariants(variants, rankings)
              }
              chosenVariants.map { variant => (variant, repositoryName, commit) }
            }
        }.toSet
        id -> latestCommitsOnly
    }
  }

  private lazy val byId = {
    cachedById.map {
      case (id, values) =>
        id -> values.map {
          case (variant, repositoryName, commit) =>
            (variant, new GitRepository(baseDir, repositoryName), commit)
        }
    }
  }

  private def locateAllIdentifiers(id: Id): Set[(VariantHash, GitRepository, Commit)] = {
    byId.getOrElse(id, throw new Exception("Cannot resolve because there is no " + id  + " in loaded repositories: " + results.map(_.repository.value).mkString(",")))
  }

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    val cacheKey = "loadVariants" + hash(id, constraints, thisUniqueId)
    usingCache(cacheKey, cache) {
      val allVariants: Set[Variant] = {
        locateAllIdentifiers(id).flatMap {
          case (hash, repository, commit) =>
            VariantMetadata.read(id, hash, repository, commit).map(_.toVariant(id))
        }
      }
      AttributeConstraintFilter.filter(id, allVariants, constraints)
    }
  }

}