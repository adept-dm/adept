package adept.repository

import adept.models._
import net.sf.ehcache.CacheManager
import net.sf.ehcache.Ehcache
import net.sf.ehcache.Element

class GitVariantsLoader(commits: Set[AdeptCommit], cacheManager: CacheManager) extends VariantsLoader {

  private val caches: Map[AdeptCommit, Ehcache] = commits.map { commit =>
    createCache(commit)
  }.toMap

  private def createCache(commit: AdeptCommit) = {
    val cacheName = commit.repo.name + "-" + commit.repo.baseDir.getAbsolutePath.hashCode + "-" + this.hashCode + "-" + commit.commit.value
    cacheManager.addCacheIfAbsent(cacheName) //TODO: if cache.exists then use
    commit -> cacheManager.getEhcache(cacheName)
  }

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    caches.flatMap {
      case (adeptCommit, cache) =>
        val repo = adeptCommit.repo
        val commit = adeptCommit.commit //FIXME: AdeptCommit is perhaps not a good name, because it makes this awkward

        val allVariants = {
          val cachedValues = cache.get(id.value)
          if (cache.isKeyInCache(id.value) && cachedValues != null) {
            cachedValues.getValue().asInstanceOf[Set[Variant]]
          } else {
            val allVariants: Set[Variant] = repo.listContent(commit.value).variantsMetadata.flatMap(_.toVariants(repo.name)).map(_._1)
            //            println("FOUND")
            //            println(allVariants.mkString("\n"))
            val element = new Element(id.value, allVariants)
            cache.put(element)
            allVariants
          }
        }
        AttributeConstraintFilter.filter(id, allVariants, constraints)
    }.toSet
  }
}