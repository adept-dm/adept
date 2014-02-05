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
    cacheManager.addCache(cacheName) //TODO: if cache.exists then use
    commit -> cacheManager.getEhcache(cacheName)
  }

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    caches.flatMap {
      case (adeptCommit, cache) =>
        val repo = adeptCommit.repo
        val commit = adeptCommit.commit //AdeptCommit is perhaps not a good name
        
        val variants = {
          val cachedValues = cache.get(id.value)
          if (cache.isKeyInCache(id.value) && cachedValues != null) {
            cachedValues.getValue().asInstanceOf[Set[Variant]]
          } else {
//            val allVariants = repo.scanFirst(commit.value){ configuredVariant => 
//              configuredVariant.id == id
//            }.flatMap{ configuredVariant =>
//              configuredVariant.toVariants.map(_._1)
//            }
//            val matchingVariants = AttributeConstraintFilter.filter(id, allVariants, constraints)
//            
//            val element = new Element(id.value, matchingVariants)
//            cache.put(element)
//            matchingVariants
          }
        }
//        variants
       Set.empty 
    }.toSet
    ???
  }
}