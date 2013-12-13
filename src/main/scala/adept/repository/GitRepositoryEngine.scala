package adept.repository

import java.io.File
import adept.core.models._
import adept.logging.Logging
import net.sf.ehcache.CacheManager
import net.sf.ehcache.Ehcache
import net.sf.ehcache.Element
import org.eclipse.jgit.api.Git

class CorruptGitRepositoryException(val repo: LocalGitRepository, val id: Id, val errorMsg: String) extends Exception(s"Corrupt git repository for $id in git repo: ${repo.repoDir}, commit: ${repo.commit}. Error message was: $errorMsg")
class GitRepositoryNotFoundException(val baseDir: File, val name: String) extends Exception(s"Could not find repository named: $name in '$baseDir'")

/**
 * The default Repository engine that writes slow, but reads fast.
 * Is thread-safe and uses a cache to be faster.
 *
 * First read uses approx the same amount of time as the FileRepository engine,
 * consecutive reads are much faster: for 1000 variants with a HD (not SSD),
 * it is about 5-20 times faster.
 *
 * When executing a complicated resolution this matters a lot.
 *
 *
 */
class GitRepositoryEngine private[repository] (override val baseDir: File, repos: Set[LocalGitRepository], cacheManager: CacheManager) extends RepositoryEngine with Logging {

  //Mutable state (must be managed carefully)
  private var repoNames: Map[String, LocalGitRepository] = repos.map(repo => repo.name -> repo).toMap
  private var caches: Map[LocalGitRepository, Ehcache] = repos.map { repo =>
    createCache(repo)
  }.toMap

  //Public methods

  def search(keyword: String): Set[(Variant, LocalGitRepository)] = {
    caches.flatMap {
      case (repo, cache) => repo.search(".*" + keyword + ".*").map(_ -> repo)
    }.toSet
  }

  def get(repoName: String, id: Id, constraints: Set[Constraint]): Set[Variant] = {
    usingRepo(repoName) { repo =>
      val cache = caches(repo)
      get(repo, cache, id, constraints)
    }
  }

  def get(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    caches.flatMap {
      case (repo, cache) => get(repo, cache, id, constraints)
    }.toSet
  }

  def rm(repoName: String, variant: Variant): Either[String, File] = {
    usingRepo(repoName) { repo =>
      repo.deleteVariant(variant)
    }
  }

  def addVariant(repoName: String, variant: Variant): Either[String, File] = {
    usingRepo(repoName) { repo =>
      //check artifact refs and artifact 
      if (variant.id.value.size > Repository.MaxIdLength) Left(s"Cannot have Id longer than ${Repository.MaxIdLength}. Variant: $variant had an Id which had ${variant.id.value.size} chars.")
      else repo.writeVariant(variant)
    }
  }

  def addArtifact(repoName: String, artifact: Artifact, file: File): Either[String, File] = {
    usingRepo(repoName) { repo =>
      repo.writeArtifactDescriptor(artifact)
    }
  }

  def getArtifacts(variant: Variant, constraints: Set[Constraint]): Set[(Artifact, Set[ArtifactRef])] = {
    val refs = variant.artifacts.filter { artifact =>
      logic.matches(artifact.attributes, constraints)
    }

    refs.groupBy(_.hash).flatMap {
      case (hash, refs) =>
        caches.flatMap {
          case (repo, cache) =>
            if (cache.isKeyInCache(hash.value)) {
              val cachedValues = cache.get(hash.value)
              Some(cachedValues.getValue().asInstanceOf[(Artifact, Set[ArtifactRef])])
            } else {
              if (repo.hasArtifactDescriptor(hash)) {
                repo.readArtifactDescriptor(hash) match {
                  case Right(artifact) =>
                    val result = artifact -> refs
                    val element = new Element(hash.value, result)
                    cache.put(element)
                    Some(result)
                  case Left(errorMsg) =>
                    throw new CorruptGitRepositoryException(repo, variant.id, s"Got unexpected error while looking for artifacts for $hash of $variant in ${repo.repoDir}: $errorMsg ")
                }
              } else None
            }
        }
    }.toSet
  }

  /** Commit all Git repositories managed by this engine and update cache(s) */
  def commit(msg: String): Set[LocalGitRepository] = synchronized {
    repoNames.map {
      case (_, repo) =>
        executeCommit(msg, repo)
    }.toSet
  }

  def commit(repoName: String, msg: String): LocalGitRepository = synchronized {
    repoNames.get(repoName) match {
      case Some(repo) => executeCommit(msg, repo)
      case None => throw new GitRepositoryNotFoundException(baseDir, repoName)
    }
  }

  //Private methods

  private def usingRepo[A](repoName: String)(f: LocalGitRepository => A): A = {
    repoNames.get(repoName) match {
      case Some(repo) => f(repo)
      case None => throw new GitRepositoryNotFoundException(baseDir, repoName)
    }
  }

  private def createCache(repo: LocalGitRepository) = {
    val cacheName = repo.name + "-" + repo.baseDir.getAbsolutePath.hashCode + "-" + this.hashCode + "-" + repo.commit.value
    cacheManager.addCache(cacheName)
    repo -> cacheManager.getEhcache(cacheName)
  }

  private def executeCommit(msg: String, repo: LocalGitRepository): LocalGitRepository = {
    if (repo.isClean) {
      repo
    } else {
      val newRepo = repo.commit(msg)
      if (repo.commit != newRepo.commit) {
        val cache = caches(repo)
        caches += createCache(newRepo)
        repoNames += newRepo.name -> newRepo //update repository names
        cacheManager.removeCache(cache.getName()) //This class created this cache and it is private so that means nothing else should be using this cache
        caches -= repo
        newRepo
      } else {
        repo
      }
    }

  }

  private def get(repo: LocalGitRepository, cache: Ehcache, id: Id, constraints: Set[Constraint]): Set[Variant] = {
    val variants =
      if (cache.isKeyInCache(id.value)) {
        val cachedValues = cache.get(id.value)
        cachedValues.getValue().asInstanceOf[Set[Variant]]
      } else {
        logger.debug(s"Cache miss on $id so loading from disk...")
        repo.readVariants(id) match {
          case Right(variants) =>
            val element = new Element(id.value, variants)
            cache.put(element)
            variants
          case Left(errorMsg) =>
            throw new CorruptGitRepositoryException(repo, id, errorMsg)
        }
      }

    logic.filter(id, variants, constraints)
  }

}