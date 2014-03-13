package adept.repository

import adept.resolution.models._
import adept.repository.models._
import adept.repository.serialization.VariantMetadata
import java.io.File
import net.sf.ehcache.CacheManager
import adept.utils.Hasher
import net.sf.ehcache.Ehcache
import adept.repository.serialization.Order
import org.eclipse.jgit.lib.ProgressMonitor

class GitLoader(baseDir: File, repositories: Set[(RepositoryInfo, RepositoryLocations)], progress: ProgressMonitor, cacheManager: CacheManager) extends VariantsLoader {
  private val thisUniqueId = Hasher.hash(repositories.map { case (ri, loc) => ri.id + "-" + ri.repository + "-" + ri.commit + "-" + ri.variant.value + "-" + loc.uris.toSeq.sorted.mkString(";") }.toSeq.sorted.mkString("#").getBytes)

  private val cache: Ehcache = {
    val cacheName = thisUniqueId
    cacheManager.addCacheIfAbsent(cacheName)
    cacheManager.getEhcache(cacheName)
  }

  import CacheHelpers._
  private def hash(id: Id, constraints: Set[Constraint]): String = {
    val uniqueString = (id.value + constraints.toSeq.sorted.map(c => c.name + c.values.mkString(",").mkString(";")))
    Hasher.hash(uniqueString.getBytes)
  }

  private val branch = "master" //TODO: manage branches in repository info
  
//  private lazy val byId = repositories.groupBy { case (r, _) => r.id }.map {
//    case (id, repositoryInfos) => id -> repositoryInfos.flatMap {
//      case (repositoryInfo, location) =>
//        val repository = new GitRepository(baseDir, repositoryInfo.repository, progress)
//        val commit = repositoryInfo.commit
//        if (!repository.hasCommit(commit)) {
//          location.uris.foreach { uri =>
//            repository.fetchRemote(uri)
//            repository.checkout(branch)
//          }
//        }
//        val matches: Set[(VariantHash, GitRepository, Commit)] = Order.firstMatch(id, repositoryInfo.variants.hashes, repository, repositoryInfo.commit) match {
//          case Some(FilePosition(variantSet, _)) => variantSet.hashes.map { variantHash =>
//            (variantHash, repository, commit)
//          }
//          case None => Set.empty
//        }
//        matches
//    }
//  }

  private def locateAllIdentifiers(id: Id): Set[(VariantHash, GitRepository, Commit)] = {
//    byId(id)
    ???
  }

  def loadVariants(id: Id, constraints: Set[Constraint]): Set[Variant] = {
    val cacheKey = "v" + hash(id, constraints)
    usingCache(cacheKey, cache) {
      val allVariants: Set[Variant] = {
        locateAllIdentifiers(id).flatMap {
          case (hash, repository, commit) =>
            VariantMetadata.read(id, hash, repository, commit)
        }
      }
      AttributeConstraintFilter.filter(id, allVariants, constraints)
    }
  }

}