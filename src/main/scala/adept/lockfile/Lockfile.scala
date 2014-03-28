package adept.lockfile

import adept.resolution.models.Id
import adept.repository.models.RepositoryLocations
import adept.repository.models.Commit
import adept.resolution.models.Constraint
import adept.artifact.models.Artifact
import adept.resolution.models.Requirement
import java.io.File
import play.api.libs.json._
import play.api.libs.functional.syntax._
import adept.repository.serialization.MetadataContent
import adept.artifact.models.ArtifactHash
import adept.artifact.models.ArtifactRef
import adept.artifact.models.ArtifactAttribute
import adept.artifact.ArtifactCache
import adept.artifact.Downloader
import org.eclipse.jgit.lib.ProgressMonitor
import scala.concurrent.future
import scala.util.Success
import adept.logging.Logging
import scala.util.Failure
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration
import adept.resolution.resolver.models.ResolveResult
import adept.repository.serialization.ArtifactMetadata
import adept.repository.models.RepositoryName
import net.sf.ehcache.CacheManager
import adept.utils.CacheHelpers
import adept.utils.Hasher
import net.sf.ehcache.Ehcache
import adept.repository.GitRepository
import adept.repository.models.ResolutionResult
import adept.repository.serialization.RepositoryLocationsMetadata
import adept.repository.Repository
import adept.repository.models.VariantHash
import adept.repository.GitHelpers
import java.io.FileInputStream

case class Lockfile(requirements: Seq[LockfileRequirement], artifacts: Seq[LockfileArtifact]) {

  lazy val jsonString = Json.prettyPrint(Json.toJson(this))

  def write(file: File): File = {
    MetadataContent.write(jsonString, file)
  }
}

object Lockfile extends Logging {
  private[adept] implicit val formatLockfile: Format[Lockfile] = Json.format[Lockfile]

  private val downloader = {
    val tmpDir = Option(System.getProperty("java.io.tmpdir"))
      .getOrElse(throw new Exception("Could not find a tmp directory because java.io.tmpdir is not set"))
    new Downloader(new File(tmpDir))
  }

  private def repoKey(repositories: Set[(RepositoryName, Commit)]) = {
    repositories.map { case (name, commit) => name.value -> commit.value }.toSeq.sorted.mkString(";")
  }

  private def lookupArtifacts(baseDir: File, hash: ArtifactHash, repositories: Set[(RepositoryName, Commit)], cache: Ehcache): Set[Artifact] = {
    val key = Hasher.hash(("lookupArtifacts" + hash.value + repoKey(repositories)).getBytes)
    val artifacts = CacheHelpers.usingCache(key, cache) {
      repositories.flatMap { //TODO: .par?
        case (name, commit) =>
          val repository = new GitRepository(baseDir, name)
          ArtifactMetadata.read(hash, repository, commit).map(_.toArtifact(hash))
      }
    }
    Set() ++ artifacts
  }

  private val CacheName = "adept-cache-lockfile"

  private def getCache(cacheManager: CacheManager) = {
    cacheManager.addCacheIfAbsent(CacheName)
    cacheManager.getEhcache(CacheName)
  }

  def read(file: File): Option[Lockfile] = {
    val is = new FileInputStream(file)
    try {
      val json = Json.parse(io.Source.fromInputStream(is).getLines.mkString("\n"))
      Json.fromJson[Lockfile](json) match {
        case JsSuccess(value, _) => Some(value)
        case JsError(errors) => throw new Exception("Could parse lockfile: " + file + ". Got: " + errors)
      }
    } finally {
      is.close()
    }
  }

  def create(baseDir: File, requirements: Set[Requirement], resolutionResults: Set[ResolutionResult], result: ResolveResult, cacheManager: CacheManager) = {
    val resolutionById = getLatestResolutionResults(baseDir, resolutionResults).groupBy(_.id)
    val preciseReqs = requirements.flatMap { requirement =>
      val resolutionResults = resolutionById.getOrElse(requirement.id, throw new Exception("Cannot find resolution result for: " + requirement))
      resolutionResults.map(r => (requirement, r.repository, r.commit, r.variant))
    }
    createPrecise(baseDir, preciseReqs, resolutionResults, result, cacheManager)
  }

  private def getLatestResolutionResults(baseDir: File, resolutionResults: Set[ResolutionResult]) = {
    resolutionResults.groupBy(result => (result.id, result.repository, result.variant)).map {
      case ((id, name, variant), results) =>
        val repository = new GitRepository(baseDir, name)
        val commits = results.map(_.commit)
        val latestCommit = GitHelpers.lastestCommit(repository, commits).getOrElse(throw new Exception("Could not find one commit which is defined as the latest in: " + (id, name, variant) + " among: " + commits))
        ResolutionResult(id, name, latestCommit, variant)
    }.toSet
  }

  private def createPrecise(baseDir: File, requirements: Set[(Requirement, RepositoryName, Commit, VariantHash)], resolutionResults: Set[ResolutionResult], result: ResolveResult, cacheManager: CacheManager) = {
    val reposInfo = getLatestResolutionResults(baseDir, resolutionResults).map { result => result.repository -> result.commit }
    val lockfileArtifacts = result.state.resolvedVariants.flatMap {
      case (_, variant) =>
        val lockfileArtifacts = variant.artifacts.map { artifactRef =>
          val artifacts = lookupArtifacts(baseDir, artifactRef.hash, reposInfo, getCache(cacheManager))
          var lastSize = -1L
          var locations = Set.empty[String]
          artifacts.foreach { artifact =>
            if (lastSize > 0) assert(artifact.size == lastSize, "Found different sizes (" + artifact.size + " VS " + lastSize + " for the same artifact: " + artifactRef + " found in: " + variant)
            lastSize = artifact.size
            locations ++= artifact.locations
          }
          LockfileArtifact(artifactRef.hash, lastSize, locations.toSeq, artifactRef.attributes.toSeq, artifactRef.filename)
        }
        lockfileArtifacts
    }
    val lockfileRequirements =
      requirements.map {
        case (requirement, name, commit, variant) =>
          val repository = new GitRepository(baseDir, name)
          val locations = repository.getRemoteUri(GitRepository.DefaultRemote).toSeq
          LockfileRequirement(requirement.id, requirement.constraints.toSeq, requirement.exclusions.toSeq, name, locations.map(LockfileRepositoryLocation(_)), commit, variant)
      }

    Lockfile(lockfileRequirements.toSeq, lockfileArtifacts.toSeq)
  }

  protected implicit val executionContext = scala.concurrent.ExecutionContext.global

  def download(baseDir: File, lockfile: Lockfile, timeout: FiniteDuration, progress: ProgressMonitor) = {
    val allCurrentCachedFiles = lockfile.artifacts.map { artifact =>
      artifact -> ArtifactCache.getOrCreateExistingCacheFile(baseDir, artifact.hash, artifact.filename.getOrElse(artifact.hash.value))
    }.toMap

    val downloadProgress = allCurrentCachedFiles.exists { case (_, cacheFile) => cacheFile.isEmpty }

    if (downloadProgress) {
      val totalKBytes = allCurrentCachedFiles.foldLeft(0)(_ + _._1.size.toInt / 1024)
      val max = if (totalKBytes  >= Int.MaxValue/1024) {
        logger.warn("Wow, you are really downloading a lot here! Found more kbytes than " + Int.MaxValue/1024 + " (" + totalKBytes + "). Progress monitoring will not work as expected")
        0
      } else totalKBytes

      progress.beginTask("Downloading artifacts (kb)", max)
    }

    val futures = lockfile.artifacts.map { artifact =>
      val currentCachedFile = allCurrentCachedFiles(artifact).map { file =>
        future(artifact.toArtifact -> file)
      }
      val result = currentCachedFile.getOrElse {
        downloader.download(artifact.toArtifact)
      }
      result.onComplete {
        case Success((artifact, _)) =>
          if (downloadProgress) progress.update(artifact.size.toInt / 1024) //<- watch out for the toInt here! we are logging this though
        case Failure(exception) =>
          logger.error("Failed to get (" + exception.getCause + ") artifacts: " + lockfile.artifacts.map(_.filename).mkString(","))
      }
      result.map{ case (a, tmpFile) =>
        a -> ArtifactCache.cache(baseDir, tmpFile, artifact.hash, artifact.filename.getOrElse(artifact.hash.value))
      }
    }
    val all = Await.result(Future.sequence(futures), timeout)
    if (downloadProgress) progress.endTask()
    all
  }
}