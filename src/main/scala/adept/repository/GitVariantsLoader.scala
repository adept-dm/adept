package adept.repository

import adept.models._
import net.sf.ehcache.CacheManager
import net.sf.ehcache.Ehcache
import net.sf.ehcache.Element
import adept.repository.models.configuration.ConfiguredRequirement
import adept.repository.models.ConfiguredVariantsMetadata

class GitVariantsLoader(commits: Set[(Id, AdeptCommit)], cacheManager: CacheManager) extends VariantsLoader {

  private val thisUniqueId = Hash.calculate(commits.map { case (id, c) => id.value + c.commit.value + c.repo.name }.toSeq.sorted.mkString).value

  //we are using 1 cache here, so each time a commit is changed or something like that we invalidate this cache
  private val cache: Ehcache = {
    val cacheName = thisUniqueId
    cacheManager.addCacheIfAbsent(cacheName) //TODO: if cache.exists then use
    cacheManager.getEhcache(cacheName)
  }

  def onlyCommits(id: Id) = {
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
    val cachedValues = cache.get(id.value)
    if (cache.isKeyInCache(id.value) && cachedValues != null) {
      cachedValues.getValue().asInstanceOf[Set[Variant]]
    } else {
      val allVariants: Set[Variant] = {
        //handle commit conflicts choosing last commit
        val lastCommitForId = onlyCommits(id).sorted.lastOption
        readVariants(lastCommitForId)
      }
      val filteredVariants = AttributeConstraintFilter.filter(id, allVariants, constraints)
      val element = new Element(id.value, filteredVariants)
      cache.put(element)
      filteredVariants
    }
  }

}