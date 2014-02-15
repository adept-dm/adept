package adept.repository

import adept.models._
import net.sf.ehcache.CacheManager
import net.sf.ehcache.Ehcache
import net.sf.ehcache.Element
import adept.repository.models.configuration.ConfiguredRequirement
import adept.repository.models.ConfiguredVariantsMetadata

class GitVariantsLoader(commits: Set[AdeptCommit], cacheManager: CacheManager) extends VariantsLoader {

  private val caches: Map[AdeptCommit, Ehcache] = commits.map { commit =>
    createCache(commit)
  }.toMap

  private def createCache(commit: AdeptCommit) = {
    val cacheName = commit.repo.name + "-" + commit.repo.baseDir.getAbsolutePath.hashCode + "-" + this.hashCode + "-" + commit.commit.value
    cacheManager.addCacheIfAbsent(cacheName) //TODO: if cache.exists then use
    commit -> cacheManager.getEhcache(cacheName)
  }

  //TODO: please rename this?
  private[adept] def read(id: Id, constraints: Set[Constraint]): Set[ConfiguredVariantsMetadata] = {
    caches.flatMap {
      case (adeptCommit, cache) =>
        val repo = adeptCommit.repo
        val commit = adeptCommit.commit //FIXME: AdeptCommit is perhaps not a good name, because it makes this awkward

        repo.listContent(commit.value).variantsMetadata.filter(v => v.id == id && AttributeConstraintFilter.matches(v.attributes, constraints) ) //NOTE: we are matching on the configured variants attributes, not the individual variants attributes. I am not sure this is right 
    }.toSet
  }

  def extractRepositoryName(id: Id) = id.value.split("/").head

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    var variants = Set.empty[Variant]
    caches.foreach {
      case (adeptCommit, cache) =>
        val repo = adeptCommit.repo
        val commit = adeptCommit.commit //FIXME: AdeptCommit is perhaps not a good name, because it makes this awkward

        if (repo.name == extractRepositoryName(id)) {
          val allVariants = {
            val cachedValues = cache.get(id.value)
            if (cache.isKeyInCache(id.value) && cachedValues != null) {
              cachedValues.getValue().asInstanceOf[Set[Variant]]
            } else {
              //              cache.synchronized { //multiple put's are ok although they are unnecessary, but it is important to not interfere
              val allVariants: Set[Variant] = repo.listContent(commit.value).variantsMetadata.flatMap(_.toVariants(repo.name))
              val element = new Element(id.value, allVariants)
              cache.put(element)
              allVariants
              //              }
            }
          }
          variants ++= AttributeConstraintFilter.filter(id, allVariants, constraints)
        }
    }
    variants
  }

}