package adept.repository.metadata

import adept.repository.models.Commit
import adept.repository.models.RankId
import adept.repository.models.Ranking
import adept.repository.models.VariantHash
import adept.resolution.models.Id
import adept.repository.Repository
import adept.repository.GitRepository
import java.io.File
import adept.utils.Hasher

case class RankingMetadata(variants: Seq[VariantHash]) { //Evaluate whether is safe enough to use iterators as a perf improvement
  def toRanking(id: Id, rankId: RankId): Ranking = {
    Ranking(id, rankId, variants)
  }

  def write(id: Id, rankId: RankId, repository: Repository): File = {
    val file = repository.ensureRankingFile(id, rankId)
    MetadataContent.write(variants.map(_.value).mkString("\n"), file)
  }
}

object RankingMetadata {
  def fromRanking(ranking: Ranking): RankingMetadata = {
    RankingMetadata(ranking.variants)
  }

  def read(id: Id, rankId: RankId, repository: GitRepository, commit: Commit): Option[RankingMetadata] = {
    repository.usingRankingInputStream(id, rankId, commit) {
      case Right(Some(is)) =>
        Some(RankingMetadata(io.Source.fromInputStream(is).getLines().map { line =>
          VariantHash(line.trim())
        }.toSeq))
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read ranking file for: " + id + ", " + rankId + " for commit: " + commit + " in dir:  " + repository.dir + ". Got error: " + error)

    }
  }

  import GitRepository._
  import Repository._

  def defaultRankId(id: Id, repository: GitRepository): RankId = {
    getXRankId(id, repository, 0, 1).headOption.getOrElse { throw new Exception("Could not find a default rank id for: " + (id, repository.name)) }
  }

  def listRankIds(id: Id, repository: GitRepository, commit: Commit): Set[RankId] = {
    val rankPath = s"""$VariantsMetadataDirName$GitPathSep${id.value}"""
    val RankIdExtractionRegEx = s"""$rankPath$GitPathSep$RankingFileNamePrefix(.*?)""".r
    val rankIds = repository.usePath[RankId](Some(rankPath), commit) { path =>
      path match {
        case RankIdExtractionRegEx(id) =>
          Some(RankId(id))
        case f =>
          None
      }
    }
    rankIds
  }

  /** get N rank ids from start (could be size of active rank ids)  */
  def getXRankId(id: Id, repository: GitRepository, start: Int = 0, N: Int = 1): Set[RankId] = {
    if (N < 1) throw new IllegalArgumentException("Cannot get " + N + " new rank ids (n is smaller than 1)")
    if (start < 0) throw new IllegalArgumentException("Cannot start at " + start + " new rank ids (start is smaller than 0)")

    var seed = {
      repository.usingRevWalk { (gitRepo, revWalk) =>
        val revCommit = repository.lookup(gitRepo, revWalk, InitTag)
          .getOrElse(throw new Exception("Cannot get next rank because init tag: " + InitTag + " does not resolve a commit - is the repository " + repository.dir.getAbsolutePath + "not properly initialized?"))
        revCommit.name + id.value
      }
    }

    def createHash(lastSeed: String) = {
      Hasher.hash((lastSeed * 2 + 42).getBytes)
    }

    for (i <- 0 to start) {
      val lastSeed = seed
      seed = createHash(lastSeed)
    }
    (for (i <- 0 until N) yield {
      val lastSeed = seed
      seed = createHash(lastSeed)
      RankId(seed)
    }).toSet
  }
}