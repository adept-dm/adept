package adept.ext

import adept.repository.models.RepositoryName
import adept.resolution.models.Requirement
import adept.repository.models.Commit
import java.io.File
import adept.repository.GitRepository
import adept.lockfile.LockfileRequirement
import adept.repository.serialization.RankingMetadata
import adept.repository.models.ResolutionResult
import adept.logging.Logging
import adept.repository.AttributeConstraintFilter
import adept.repository.serialization.VariantMetadata
import adept.repository.serialization.ResolutionResultsMetadata

object Upgrade extends Logging {
  def toLatestBinaryCompatible(baseDir: File, requirements: Set[LockfileRequirement]): Set[LockfileRequirement] = {
    toLatestRanking(baseDir, requirements, AttributeDefaults.BinaryVersionAttribute)
  }

  /** @param sameAttribute is the the name of the attribute which we are upgrading on  */
  private[adept] def toLatestRanking(baseDir: File, requirements: Set[LockfileRequirement], sameAttributeName: String): Set[LockfileRequirement] = {
    requirements.map { lockfileReq =>
      val ResolutionResult(id, name, commit, variant) = lockfileReq.toResolutionResult
      val requirement = lockfileReq.toRequirement

      val repository = new GitRepository(baseDir, name)
      val rankings = RankingMetadata.listRankIds(id, repository, commit).flatMap { rankId =>
        RankingMetadata.read(id, rankId, repository, commit)
      }
      val allRankings = rankings.filter { ranking =>
        ranking.variants.contains(variant)
      }
      if (allRankings.isEmpty) {
        logger.warn("While upgrading: " + id + " in " + repository.dir.getAbsolutePath + " at " + commit + " : could not find any matching rankings")
        lockfileReq
      } else if (allRankings.size > 1) {
        throw new Exception("While upgrading: " + id + " in " + repository.dir.getAbsolutePath + " at " + commit + " : found more than one ranking: " + allRankings)
      } else {
        assert(allRankings.size == 1)
        val ranking = allRankings.head
        if (ranking.variants.isEmpty) {
          lockfileReq
        } else {
          val previousVariantAttribute = {
            VariantMetadata.read(id, variant, repository, commit) match {
              case Some(metadata) =>
                metadata.attributes.find(_.name == sameAttributeName)
              case None => throw new Exception("Cannot read variant metadata: " + (id, variant, repository.dir.getAbsolutePath, commit))
            }
          }
          val newVariant = {
            ranking.variants.find {
              case hash =>
                //give up when we found our hash
                hash == variant || { //or: take the first one that matches the constraints and has the same attribute:
                    VariantMetadata.read(id, hash, repository, commit) match {
                      case Some(metadata) =>
                        val currentVariantAttribute = metadata.attributes.find(_.name == sameAttributeName)
                         currentVariantAttribute.isDefined && //first we have to find the attribute name here, if not we do not upgrade to this 
                         (previousVariantAttribute.isDefined && previousVariantAttribute == currentVariantAttribute) && //if we found the attribute name earlier as well, well we have to check that it is the same as the one we found
                          AttributeConstraintFilter.matches(metadata.attributes.toSet, lockfileReq.constraints.toSet)
                      case None =>
                        //if you are here it means something unexpected has happened:
                        throw new Exception("Could not load a variant hash: " + hash + " from " + (id, repository.dir.getAbsolutePath(), commit))
                    }
                  }
            }.getOrElse(throw new Exception("Expected to find one (any) hash, but at least: " + variant + " in " + (id, repository.dir.getAbsolutePath(), commit)))
          }
          lockfileReq.copy(variant = newVariant)
        }

      }
    }
  }

  def followRedirect(baseDir: File, requirements: Set[LockfileRequirement]): Set[LockfileRequirement] = {
    logger.warn("following redirects is EXPERIMENTAL")
    val newReqs = toLatestBinaryCompatible(baseDir, requirements).diff(requirements)
    newReqs.map { lockfileReq =>
      val repository = new GitRepository(baseDir, lockfileReq.repository)
      val ResolutionResult(id, name, commit, hash) = lockfileReq.toResolutionResult

      VariantMetadata.read(id, hash, repository, commit) match {
        case Some(variant) =>
          variant.attributes.find(_.name == VariantRename.RedirectAttributeName) match {
            case Some(redirectAttribute) =>
              ResolutionResultsMetadata.read(id, hash, repository, commit) match {
                case Some(resolutionResults) =>
                  throw new Exception("Cannot follow redirects yet, because it is not implemented") //TODO: <--
                //resolutionResults.values.find(result => result.id == destId && result.repository == destName)
                case None =>
                  lockfileReq
              }
            case None =>
              lockfileReq
          }
        case None =>
          throw new Exception("Could not load a variant hash: " + hash + " from " + (id, repository.dir.getAbsolutePath(), commit))
      }
    }
  }
}