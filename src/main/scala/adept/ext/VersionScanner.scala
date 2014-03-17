package adept.ext

import adept.resolution.models._
import adept.repository.models._
import adept.repository._
import adept.repository.serialization._

object VersionScanner {
  def findVersion(id: Id, version: Version, repository: GitRepository, commit: Commit): Option[VariantHash] = {
    val activeIds = Order.listActiveOrderIds(id, repository, commit)
    val foundHashes = activeIds.flatMap { orderId =>
      Order.scanFirst(id, orderId, repository, commit) { hash =>

        for {
          variant <- VariantMetadata.read(id, hash, repository, commit)
          foundVersion <- {
            VersionOrder.getVersion(variant)
          } if (foundVersion == version)
        } yield {
          (orderId, VariantMetadata.fromVariant(variant).hash)
        }
      }
    }

    if (foundHashes.size > 1) throw new Exception("Could not determine one single order in which id: " + id + " version: " + version + " was found! Got: " + foundHashes)
    else foundHashes.headOption.map {
      case (_, hash) =>
        hash
    }
  }
}