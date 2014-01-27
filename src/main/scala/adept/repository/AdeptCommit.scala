package adept.repository

import adept.repository.models.Commit

case class CommitCompareException(commit1: AdeptCommit, commit2: AdeptCommit) extends Exception("Cannot compare commits: " + commit1 + " and " + commit2 + ".")

/**
 * Represents a comparable commit for a given `AdeptGitRepository`.
 */
case class AdeptCommit private[adept] (repo: AdeptGitRepository, val commit: Commit) {
  def canCompare(that: AdeptCommit): Boolean = {
    this.repo.dir == that.repo.dir && this.repo.branchName == that.repo.branchName
  }

  def compare(that: AdeptCommit): Int = {
    if (!canCompare(that)) throw CommitCompareException(this, that)
    else {

      ???
    }
  }
}