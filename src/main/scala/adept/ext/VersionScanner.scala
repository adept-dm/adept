package adept.ext

import adept.resolution.models._
import adept.repository.models._
import adept.repository._
import adept.repository.metadata._

object VersionScanner {
  def findVersion(id: Id, version: Version, repository: GitRepository, commit: Commit): Option[VariantHash] = {
    val rankIds = RankingMetadata.listRankIds(id, repository, commit)
    val foundHashes = rankIds.flatMap { rankId =>
      RankingMetadata.read(id, rankId, repository, commit).flatMap { ranking =>
        ranking.variants.find { hash =>
          val maybeHash = for {
            metadata <- VariantMetadata.read(id, hash, repository, commit)
            foundVersion <- {
              VersionRank.getVersion(metadata.toVariant(id))
            } if (foundVersion == version)
          } yield {
            metadata.hash
          }
          maybeHash.isDefined
        }.map(hash => rankId -> hash)
      }
    }

    if (foundHashes.size > 1) throw new Exception("Could not determine one single ranking in which id: " + id + " version: " + version + " was found! Got: " + foundHashes)
    else foundHashes.headOption.map {
      case (_, hash) =>
        hash
    }
  }
}