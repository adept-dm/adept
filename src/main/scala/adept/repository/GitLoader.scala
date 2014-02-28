package adept.repository

import adept.models._
import net.sf.ehcache.CacheManager
import net.sf.ehcache.Ehcache
import net.sf.ehcache.Element
import adept.repository.models.configuration.ConfiguredRequirement
import adept.repository.models.ConfiguredVariantsMetadata
import adept.repository.models.LockFileRequirement
import java.io.File
import adept.repository.models.configuration.ConfigurationId
import adept.repository.models.RepositoryMetadata
import adept.repository.models.ArtifactMetadata

object GitLoader {
  def loadCommits(baseDir: File, requirements: Set[LockFileRequirement], cacheManager: CacheManager) = {
    //TODO: make .par suited for IO
    val initCommits = requirements.map { req =>
      (req, (ConfigurationId.join(req.id, req.configuration), AdeptCommit(new AdeptGitRepository(baseDir, req.repositoryName), req.repositoryCommit)))
    } ++ requirements.map { req =>
      (req, (req.id, AdeptCommit(new AdeptGitRepository(baseDir, req.repositoryName), req.repositoryCommit)))
    }

    val commits: Set[(Id, AdeptCommit)] = {
      import collection.mutable._
      //TODO: adjust this ugly piece of code!
      val gitVariantsLoader = new GitLoader(initCommits.map(_._2), cacheManager = cacheManager)
      val rootVariantHashes = new HashSet[Hash] with SynchronizedSet[Hash]

      requirements.par.foreach { req =>
        rootVariantHashes ++= gitVariantsLoader.loadVariants(req.id, req.constraints.toSet).map(Hash.calculate)
      }
      //defined as vars to make them easy to spot
      var commits: Set[(Id, AdeptCommit)] = new HashSet[(Id, AdeptCommit)] with SynchronizedSet[(Id, AdeptCommit)] //perf boost  par
      var repositoryMetadata: Set[(LockFileRequirement, RepositoryMetadata)] = new HashSet[(LockFileRequirement, RepositoryMetadata)] with SynchronizedSet[(LockFileRequirement, RepositoryMetadata)]

      initCommits.par.foreach {
        case (req, current @ (commitId, c)) =>
          commits += current
          //TODO: cache this:
          c.repo.listContent(c.commit.value).repositoryMetadata.foreach { repositoryMetadata =>
            if (repositoryMetadata.variants.exists(h => rootVariantHashes.contains(h))) { //i.e. there is repository variant represents one or more of the rootVariants
              commits ++= repositoryMetadata.load(baseDir, req.id, req.configuration)
            }
          }
      }
      commits.toSet
    }
    commits
  }
}

class GitLoader(commits: Set[(Id, AdeptCommit)], cacheManager: CacheManager) extends VariantsLoader {
  lazy val onlyCommits = commits.map(_._2)

  def getArtifacts(hashes: Set[Hash]) = {
    var artifacts = {
      import collection.mutable._
      new HashSet[Artifact] with SynchronizedSet[Artifact] //perf boost  par
    }
    onlyCommits.foreach { c =>
      val cacheKey = "ca" + c.commit.value + c.repo.name + c.repo.dir.getAbsolutePath
      val cachedValues = cache.get(cacheKey)
      val artifactMetadata = if (cache.isKeyInCache(cacheKey) && cachedValues != null) {
        cachedValues.getValue().asInstanceOf[Set[ArtifactMetadata]]
      } else {
        c.repo.listContent(c.commit.value).artifactsMetadata
      }
      artifactMetadata.foreach { am =>
        if (hashes(am.hash))
          artifacts += am.toArtifact
      }
    }
    artifacts.toSet
  }

  private val thisUniqueId = Hash.calculate(commits.map { case (id, c) => id.value + c.commit.value + c.repo.name }.toSeq.sorted.mkString).value

  //we are using 1 cache here, so each time a commit is changed or something like that we invalidate this cache
  private val cache: Ehcache = {
    val cacheName = thisUniqueId
    cacheManager.addCacheIfAbsent(cacheName) //TODO: if cache.exists then use
    cacheManager.getEhcache(cacheName)
  }

  def getCommitsForId(id: Id) = {
    var onlyCommits = Vector.empty[AdeptCommit] //perf boost (avoid multiple traversals)
    commits.foreach { c =>
      if (c._1 == id) onlyCommits +:= c._2
    }
    onlyCommits
  }

  def readVariants(lastCommitForId: Option[AdeptCommit]) = {
    import collection.mutable._
    var variants: Set[Variant] = new HashSet[Variant] with SynchronizedSet[Variant] //perf boost  par 
    lastCommitForId.foreach { c =>
      c.repo.listContent(c.commit.value).variantsMetadata.par.foreach { vm =>
        variants ++= vm.toVariants
      }
    }
    variants.toSet
  }

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    val cacheKey = "v" + Hash.calculate(id, constraints).value
    val cachedValues = cache.get(cacheKey)
    if (cache.isKeyInCache(cacheKey) && cachedValues != null) {
      cachedValues.getValue().asInstanceOf[Set[Variant]]
    } else {
      val allVariants: Set[Variant] = {
        //handle commit conflicts choosing last commit
        val lastCommitForId = getCommitsForId(id).sorted.lastOption
        readVariants(lastCommitForId)
      }
      val filteredVariants = AttributeConstraintFilter.filter(id, allVariants, constraints)
      val element = new Element(cacheKey, filteredVariants)
      cache.put(element)
      filteredVariants
    }
  }

}