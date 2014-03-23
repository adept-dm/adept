package adept.repository

import adept.repository.models.Ranking
import adept.resolution.models.Id
import adept.repository.models.VariantHash
import adept.repository.models.Commit
import adept.repository.serialization.RankingMetadata

object RankLogic {
  val Default = new RankLogic
}

class RankLogic {
  
  /** Defines rank logic */
  def chosenVariants(variants: Set[VariantHash], rankings: Set[Ranking]): Set[VariantHash] = {
    var comparableVariants = variants
    rankings.foreach { ranking =>
      var foundVariantHash: Option[VariantHash] = None
      var first = ranking.variants.headOption
      ranking.variants.foreach { hash =>
        if (variants.contains(hash)) {
          if (foundVariantHash.isEmpty) foundVariantHash = Some(hash) //found a hash, is considered the best of which ever hashes are found in this file
          else if (foundVariantHash.isDefined) comparableVariants -= hash //there is already a hash which is considered better so remove this one
        }
      }
      if (foundVariantHash.isEmpty) comparableVariants ++= first //we did not find any matches in this file so pick first
    }
    comparableVariants
  }

  // Utility methods:
  def getActiveVariants(id: Id, repository: GitRepository, commit: Commit) = {
    getChosenVariants(id, Set.empty, repository, commit)
  }

  def getChosenVariants(id: Id, variants: Set[VariantHash], repository: GitRepository, commit: Commit) = {
    val rankings = RankingMetadata.listRankIds(id, repository, commit).flatMap { rankId =>
      RankingMetadata.read(id, rankId, repository, commit).map(_.toRanking(id, rankId))
    }
    chosenVariants(variants, rankings)
  }

  def activeVariants(rankings: Set[Ranking]): Set[VariantHash] = {
    chosenVariants(Set.empty, rankings)
  }
  
}