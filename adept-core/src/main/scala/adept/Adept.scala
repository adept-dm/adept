package adept

import java.io.{File, FileOutputStream}

import adept.lockfile.{Lockfile, LockfileConverters}
import adept.logging.Logging
import adept.models.GitSearchResult
import adept.repository.{AttributeConstraintFilter, GitLoader, GitRepository, Repository}
import adept.repository.metadata.{RankingMetadata, VariantMetadata}
import adept.repository.models.{ContextValue, RepositoryName}
import adept.resolution.Resolver
import adept.resolution.models.{Constraint, Id, Requirement, Variant}
import net.sf.ehcache.CacheManager

class Adept(baseDir: File, cacheManager: CacheManager, passphrase: Option[String] = None) extends Logging {

  private[adept] def matches(term: String, id: Id) = {
    (id.value + Id.Sep).contains(term)
  }

  def localResolve(requirements: Set[Requirement], inputContext: Set[ContextValue], overriddenInputContext:
  Set[ContextValue], overriddenContext: Set[ContextValue], providedVariants: Set[Variant], overrides:
                   Set[ContextValue] = Set.empty, unversionedBaseDirs: Set[File] = Set.empty) = {
    val loader = new GitLoader(baseDir, overriddenContext, cacheManager = cacheManager, unversionedBaseDirs =
      unversionedBaseDirs, loadedVariants = providedVariants)
    val resolver = new Resolver(loader)
    val result = resolver.resolve(requirements)
    if (result.isResolved) Right(result)
    else Left(result)
  }

  def writeLockfile(lockfile: Lockfile, file: File) = {
    val fos: FileOutputStream = null
    try {
      val fos = new FileOutputStream(file)
      fos.write(LockfileConverters.toJsonString(lockfile).getBytes)
      fos.flush()
    } finally {
      if (fos != null) fos.close()
    }
  }

  def searchLocalRepository(term: String, name: RepositoryName, constraints: Set[Constraint] = Set.empty):
  Set[GitSearchResult] = {
    logger.debug(s"Searching repository $name for package $term")
    val repository = new GitRepository(baseDir, name)
    if (repository.exists) {
      logger.debug("Repository exists")
      val commit = repository.getHead
      VariantMetadata.listIds(repository, commit).flatMap { id =>
        if (matches(term, id)) {
          logger.debug(s"Variant $id matches package ($term)")
          val locations = repository.getRemoteUri(GitRepository.DefaultRemote).map { location =>
            Seq(location)
          }.getOrElse(Seq.empty)
          val variants = RankingMetadata.listRankIds(id, repository, commit).flatMap { rankId =>
            val ranking = RankingMetadata.read(id, rankId, repository, commit)
              .getOrElse(throw new Exception("Could not read rank id: " + (id, rankId,
              repository.dir.getAbsolutePath, commit)))
            ranking.variants.map { hash =>
              VariantMetadata.read(id, hash, repository, commit).map(_.toVariant(id))
                .getOrElse(throw new Exception("Could not read variant: " + (rankId, id, hash,
                repository.dir.getAbsolutePath, commit)))
            }.filter { variant =>
              constraints.isEmpty ||
                AttributeConstraintFilter.matches(variant.attributes.toSet, constraints)
            }.map {
              _ -> rankId
            }
          }

          variants.map {
            case (variant, rankId) =>
              GitSearchResult(variant, rankId, repository.name, commit, locations, isLocal = true)
          }
        } else {
          Set.empty[GitSearchResult]
        }
      }
    } else {
      logger.debug("Repository doesn't exist")
      Set.empty[GitSearchResult]
    }
  }

  def localSearch(term: String, constraints: Set[Constraint] = Set.empty): Set[GitSearchResult] = {
    logger.debug(s"Searching locally for package $term in '$baseDir'")
    Repository.listRepositories(baseDir).flatMap { name =>
      searchLocalRepository(term, name, constraints)
    }
  }
}
