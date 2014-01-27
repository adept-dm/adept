package adept.repository

import adept.repository.models.Commit

/**
 * Represents a comparable commit for a given `AdeptGitRepository`.
 */
class AdeptCommit private[adept] (repo: AdeptGitRepository, val commit: Commit) {
  def canCompare(that: AdeptCommit) = ???
  def compare(that: AdeptCommit): Int = ???
}