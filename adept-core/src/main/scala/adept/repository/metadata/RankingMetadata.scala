package adept.repository.metadata

import adept.repository.models.Commit
import adept.repository.models.RankId
import adept.repository.models.Ranking
import adept.repository.models.VariantHash
import adept.resolution.models.Id
import adept.repository.Repository
import adept.repository.GitRepository
import java.io.File
import adept.hash.Hasher
import java.io.BufferedReader
import java.io.InputStreamReader

case class RankingMetadata(variants: Seq[VariantHash]) { //Evaluate whether is safe enough to use iterators as a perf improvement
  def toRanking(id: Id, rankId: RankId): Ranking = {
    Ranking(id, rankId, variants)
  }

  def write(id: Id, rankId: RankId, repository: Repository): File = {
    val file = repository.ensureRankingFile(id, rankId)
    if (variants.distinct != variants) throw new Exception("Cannot write ranking file because there several similar rankids: " + id + " rank id: " + rankId + " in " + repository.dir.getAbsolutePath())
    MetadataContent.write(variants.map(_.value).mkString("\n"), file)
  }
}

object RankingMetadata {
  def fromRanking(ranking: Ranking): RankingMetadata = {
    RankingMetadata(ranking.variants)
  }

  def read(id: Id, rankId: RankId, repository: Repository): Option[RankingMetadata] = {
    val file = repository.getRankingFile(id, rankId)
    repository.usingFileInputStream(file) {
      case Right(Some(is)) =>
        //TODO: hitting a Stream closed exception when using io.Source.fromInputStream(is).getLines() here?
        val isReader = new InputStreamReader(is)
        val reader = new BufferedReader(isReader)
        try {
          var line = reader.readLine
          var hashes = Seq.empty[VariantHash]
          while (line != null) {
            hashes +:= VariantHash(line.trim()) //prepend
            line = reader.readLine()
          }
          Some(RankingMetadata(hashes.reverse)) //first one added, is highest ranked
        } finally {
          isReader.close()
          reader.close()
        }
      case Right(None) => None
      case Left(error) =>
        throw new Exception("Could not read file: " + file.getAbsolutePath + " for id: " + id + " rank id: " + rankId + ". Got error: " + error)
    }
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

  val DefaultRankId = RankId("default")

  def listRankIds(id: Id, repository: GitRepository, commit: Commit): Set[RankId] = {
    val rankPath = s"""$VariantsMetadataDirName$GitPathSep${id.value}"""
    val RankIdExtractionRegEx = s"""^$rankPath$GitPathSep([^/]*?)\\.$RankingFileEnding$$""".r //do not match on /
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

  private[repository] def findRankingFiles(repository: Repository) = {
    def walkTree(file: File): Iterable[File] = {
      val children = new Iterable[File] {
        def iterator = if (file.isDirectory) file.listFiles.iterator else Iterator.empty
      }
      Seq(file) ++: children.flatMap(walkTree(_))
    }

    for (f <- walkTree(repository.dir) if f.getName.endsWith(Repository.RankingFileEnding)) yield f
  }

  def listRankIds(id: Id, repository: Repository): Set[RankId] = {
    val RankIdExtractionRegEx = s"""([^/]*?).$RankingFileEnding$$""".r //do not match on / because commit version does not do that either
    val rankIds = findRankingFiles(repository).flatMap { file =>
      file.getName match {
        case RankIdExtractionRegEx(id) =>
          Some(RankId(id))
        case f =>
          throw new Exception("Expected " + file.getAbsolutePath() + " to be a ranking file but it was not")
      }
    }.toSet
    rankIds
  }

}