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

case class LockfileRepositoryLocation(value: String)

case class LockfileRequirement(id: Id, constraints: Seq[Constraint], exclusions: Seq[Id], repository: RepositoryName, locations: Seq[LockfileRepositoryLocation], commit: Commit) {
  def toRequirement: (Requirement, RepositoryName, Seq[LockfileRepositoryLocation], Commit) = {
    (Requirement(id, constraints.toSet, exclusions.toSet), repository, locations, commit)
  }
}

case class LockfileArtifact(hash: ArtifactHash, size: Long, locations: Seq[String], attributes: Seq[ArtifactAttribute], filename: Option[String]) {
  def toArtifact = Artifact(hash, size, locations.toSet)
  def toArtifactRef = ArtifactRef(hash, attributes.toSet, filename)
}

object LockfileRequirement {

  def fromRequirement(requirement: Requirement, repository: RepositoryName, locations: RepositoryLocations, commit: Commit) = {
    LockfileRequirement(requirement.id, requirement.constraints.toSeq.sorted, requirement.exclusions.toSeq.sortBy(_.value), repository, locations.uris.toSeq.map(LockfileRepositoryLocation(_)), commit)
  }
}

case class Lockfile(requirements: Seq[LockfileRequirement], artifacts: Seq[LockfileArtifact]) {

  lazy val jsonString = Json.prettyPrint(Json.toJson(this))

  def write(file: File): File = {
    MetadataContent.write(jsonString, file)
  }
}

object Lockfile extends Logging {

  private[adept] implicit val formatLockfileRequirement: Format[LockfileRequirement] = {
    (
      (__ \ "id").format[String] and
      (__ \ "constraints").format[Map[String, Set[String]]] and
      (__ \ "exclusions").format[Seq[String]] and
      (__ \ "repository").format[String] and
      (__ \ "locations").format[Seq[String]] and
      (__ \ "commit").format[String])({
        case (id, constraints, exclusions, repository, locations, commit) =>
          LockfileRequirement(
            Id(id),
            constraints.map { case (name, values) => Constraint(name, values) }.toSeq,
            exclusions.map(Id(_)).toSeq,
            RepositoryName(repository),
            locations.map(LockfileRepositoryLocation(_)).toSeq,
            Commit(commit))
      },
        unlift({ r: LockfileRequirement =>
          val LockfileRequirement(id, constraints, exlusions, repository, locations, commit) = r
          Some((
            id.value,
            constraints.toSeq.sorted.map(c => c.name -> c.values).toMap,
            exlusions.toSeq.map(_.value).sorted,
            repository.value,
            locations.toSeq.map(_.value).sorted,
            commit.value))
        }))
  }

  private[adept] implicit val formatLockfileArtifact: Format[LockfileArtifact] = {
    (
      (__ \ "hash").format[String] and
      (__ \ "size").format[Long] and
      (__ \ "locations").format[Seq[String]] and
      (__ \ "attributes").format[Map[String, Set[String]]] and
      (__ \ "filename").format[Option[String]])({
        case (hash, size, locations, attributes, filename) =>
          LockfileArtifact(ArtifactHash(hash), size, locations, attributes.map { case (name, values) => ArtifactAttribute(name, values) }.toSeq, filename)
      }, unlift({ a: LockfileArtifact =>
        val LockfileArtifact(hash, size, locations, attributes, filename) = a
        Some((hash.value, size, locations, attributes.map(a => a.name -> a.values).toMap, filename))
      }))
  }

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

  def create(baseDir: File, requirements: Set[Requirement], resolutionResults: Set[ResolutionResult], result: ResolveResult, cacheManager: CacheManager) = {
    val resolutionById = resolutionResults.groupBy(_.id)
    val preciseReqs = requirements.flatMap{ requirement => 
      val resolutionResults = resolutionById.getOrElse(requirement.id, throw new Exception("Cannot find resolution result for: " + requirement))
      resolutionResults.map(r => (r.repository, requirement, r.commit) )
    }
    createPrecise(baseDir, preciseReqs, resolutionResults, result, cacheManager)
  }
  
  private def createPrecise(baseDir: File, requirements: Set[(RepositoryName, Requirement, Commit)], resolutionResults: Set[ResolutionResult], result: ResolveResult, cacheManager: CacheManager) = {
    val reposInfo = resolutionResults.map { result => result.repository -> result.commit }
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
      requirements.map { case (name, requirement, commit) =>
        val repository = new GitRepository(baseDir, name)
        val locations = repository.getRemoteUri(GitRepository.DefaultRemote).toSeq
        LockfileRequirement(requirement.id, requirement.constraints.toSeq, requirement.exclusions.toSeq, name, locations.map(LockfileRepositoryLocation(_)), commit)
      }
    
    Lockfile(lockfileRequirements.toSeq, lockfileArtifacts.toSeq)
  }

  protected implicit val executionContext = scala.concurrent.ExecutionContext.global

  def download(baseDir: File, lockfile: Lockfile, timeout: FiniteDuration, progress: ProgressMonitor) = {
    val totalBytes = lockfile.artifacts.foldLeft(0L)(_ + _.size)
    val max = if (totalBytes > Int.MaxValue) {
      logger.warn("Wow, you are really downloading a lot here! While downloading lockfile got total bytes higher than " + Int.MaxValue + " (" + totalBytes + "). Lockfile:\n" + lockfile.jsonString)
      0
    } else totalBytes.toInt //<- toInt
    progress.beginTask("Downloading artifacts", max)
    val futures = lockfile.artifacts.map { artifact =>
      val currentCachedFile = ArtifactCache.getOrCreateExistingCacheFile(baseDir, artifact.hash, artifact.filename.getOrElse(artifact.hash.value)).map { file =>
        future(artifact.toArtifact -> file)
      }
      val result = currentCachedFile.getOrElse {
        downloader.download(artifact.toArtifact)
      }
      result.onComplete {
        case Success((a, _)) =>
          progress.update(a.size.toInt) //<- watch out for the toInt here! we are logging this though
        case Failure(e) =>
          logger.error("Failed to download (" + e.getCause + ") artifacts: " + lockfile.artifacts.map(_.filename).mkString(","))
      }
      result
    }
    val allDownloaded = Await.result(Future.sequence(futures), timeout)
    progress.endTask()
    allDownloaded
  }
}