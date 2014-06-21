package adept

import java.io.File
import net.sf.ehcache.CacheManager
import org.eclipse.jgit.lib.ProgressMonitor
import adept.logging.Logging
import adept.resolution.models.Id
import adept.repository.models.RepositoryName
import adept.resolution.models.Constraint
import adept.repository.GitRepository
import adept.repository.metadata.VariantMetadata
import adept.repository.models.RepositoryLocations
import adept.repository.metadata.RankingMetadata
import org.eclipse.jgit.lib.TextProgressMonitor
import adept.repository.AttributeConstraintFilter
import adept.repository.Repository
import adepthub.models.GitSearchResult
import adept.lockfile.Lockfile
import java.io.FileOutputStream
import adept.lockfile.LockfileConverters
import adept.repository.models.ResolutionResult
import adept.resolution.models.Variant
import adept.resolution.models.Requirement
import adept.repository.GitLoader
import adept.resolution.Resolver

//TODO: move adept-core
class Adept(baseDir: File, cacheManager: CacheManager, passphrase: Option[String] = None, progress: ProgressMonitor = new TextProgressMonitor) extends Logging {

  private[adept] def matches(term: String, id: Id) = {
    (id.value + Id.Sep).contains(term)
  }

  def localResolve(requirements: Set[Requirement], inputContext: Set[ResolutionResult], overriddenInputContext: Set[ResolutionResult], overriddenContext: Set[ResolutionResult], providedVariants: Set[Variant], overrides: Set[ResolutionResult] = Set.empty, unversionedBaseDirs: Set[File] = Set.empty) = {
    val loader = new GitLoader(baseDir, overriddenContext, cacheManager = cacheManager, unversionedBaseDirs = unversionedBaseDirs, loadedVariants = providedVariants, progress = progress)
    val resolver = new Resolver(loader)
    val result = resolver.resolve(requirements)
    if (result.isResolved) Right(result)
    else Left(result)
  }

  def writeLockfile(lockfile: Lockfile, file: File) = {
    var fos: FileOutputStream = null
    try {
      var fos = new FileOutputStream(file)
      fos.write(LockfileConverters.toJsonString(lockfile).getBytes)
      fos.flush()
    } finally {
      if (fos != null) fos.close()
    }
  }

  def searchLocalRepository(term: String, name: RepositoryName, constraints: Set[Constraint] = Set.empty): Set[GitSearchResult] = {
    val repository = new GitRepository(baseDir, name)
    if (repository.exists) {
      val commit = repository.getHead
      VariantMetadata.listIds(repository, commit).flatMap { id =>
        if (matches(term, id)) {
          val locations = repository.getRemoteUri(GitRepository.DefaultRemote).map { location =>
            Seq(location)
          }.getOrElse(Seq.empty)
          val variants = RankingMetadata.listRankIds(id, repository, commit).flatMap { rankId =>
            val ranking = RankingMetadata.read(id, rankId, repository, commit)
              .getOrElse(throw new Exception("Could not read rank id: " + (id, rankId, repository.dir.getAbsolutePath, commit)))
            ranking.variants.map { hash =>
              VariantMetadata.read(id, hash, repository, commit).map(_.toVariant(id))
                .getOrElse(throw new Exception("Could not read variant: " + (rankId, id, hash, repository.dir.getAbsolutePath, commit)))
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
      Set.empty[GitSearchResult]
    }
  }

  def localSearch(term: String, constraints: Set[Constraint] = Set.empty): Set[GitSearchResult] = {
    Repository.listRepositories(baseDir).flatMap { name =>
      searchLocalRepository(term, name, constraints)
    }
  }
}
