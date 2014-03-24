package adept.repository

import adept.repository.models.Ranking
import adept.resolution.models.Id
import adept.repository.models.VariantHash
import adept.repository.models.Commit
import adept.repository.serialization.RankingMetadata
import adept.repository.models.RankId

object RankLogic {

  /** Defines rank logic:
   *  - if the list of input variants contains 2 or more that is in the same ranking file, select the first one
   *  - if there is no variant hashes found for the input variants in the rankings, select the last one in the ranking file
   *  
   *  Also checks that 2 rankings does NOT contain the same hash
   */
  def chosenVariants(variants: Set[VariantHash], rankings: Set[Ranking]): Set[VariantHash] = {
    var rankIds = Set.empty[RankId]
    var comparableVariants = variants
    if (rankings.toSeq.flatMap(_.variants).size != rankings.flatMap(_.variants).toSet.size) {
      throw new Exception("Found multiple ranking files that have the same hash:" + rankings.mkString("\n"))
    } 
    rankings.foreach { ranking =>
      if (rankIds.contains(ranking.rankId)) throw new Exception("Could not chose variants, because there are multiple equal rank ids in rankings: " + rankings.map(_.rankId))
      rankIds += ranking.rankId

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

  //Include these?
  //  def newRanking(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit) = {
  //    val current = RankingMetadata.listRankIds(id, repository, commit)
  //    val rankId = RankingMetadata.getXRankId(id, repository, current.size, 1).headOption.value
  //    RankingMetadata(Seq(hash)).write(id, rankId, repository)
  //  }
  //
  //  def findRankings(id: Id, hash: VariantHash, repository: GitRepository, commit: Commit) = {
  //    val rankings = RankingMetadata.listRankIds(id, repository, commit).flatMap { rankId =>
  //      RankingMetadata.read(id, rankId, repository, commit).map(_.toRanking(id, rankId))
  //    }
  //    val matchingRankings = rankings.filter(_.variants.contains(hash))
  //    matchingRankings should have size (1)
  //    matchingRankings.headOption.value
  //  }
  //
  //  def prepend(id: Id, rankId: RankId, hash: VariantHash, repository: GitRepository, commit: Commit) = {
  //    val rankingMetadata = RankingMetadata.read(id, rankId, repository, commit).headOption.value
  //    rankingMetadata.copy(variants = hash +: rankingMetadata.variants).write(id, rankId, repository)
  //  }
}