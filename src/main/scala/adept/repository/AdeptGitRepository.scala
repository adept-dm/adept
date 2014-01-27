package adept.repository

import java.io.File
import adept.repository.models._
import adept.logging.Logging
import org.eclipse.jgit.lib.Constants

object AdeptGitRepository {

  //avoid cache collisions with hashes which are 64 chars
  val MaxIdLength = 63

  val ArtifactDescriptorDirName = "artifacts"
  val VariantsDirName = "variants"
  val RewritesDirName = "rewrites"
  val ModificiationsFileName = "modifications"
  val ReposDirName = "repos"

  val JsonFileEnding = "json"

  val InitTag = "init"
  val MasterBranchName = "master"
  val Head = Constants.HEAD

  val IdDirSep = "/" //the character in an ID that indicates a different directory

  def getReposDir(baseDir: File) = new File(baseDir, ReposDirName)
  def getRepoDir(baseDir: File, name: String) = new File(getReposDir(baseDir), name)
  def getArtifactDescriptorsDir(baseDir: File, name: String) = new File(getRepoDir(baseDir, name), ArtifactDescriptorDirName)
  def getVariantsDir(baseDir: File, name: String) = new File(getRepoDir(baseDir, name), VariantsDirName)
  def getModificationsFile(baseDir: File, name: String) = {
    val rewritesDir = new File(getRepoDir(baseDir, name), RewritesDirName)
    new File(rewritesDir, ModificiationsFileName)
  }

}

/**
 * Unsurprisingly this class represent a Git repository in the context of Adept.
 *
 * In most cases it is only a wrapper for a Git repository.
 * In the odd case it extends Git with some specifics for Adept.
 * This is the reason why it is called _Adept_GitRepository, not just GitRepository.
 *  
 * These "odd" cases occurs whenever "rewrites" are required:
 *   - If you want to add metadata to a commit that is not HEAD
 *   - If you want to remove metadata on a commit that is not HEAD
 *  
 * In which case AdeptGitRepository adds the mapping of old commits to 
 * the new commits in  a file (see `getModificationsFile`). This is required to be able 
 * to _always_ be able to compare commits in the same repository, even 
 * though the history has been rewritten since.
 */
class AdeptGitRepository(val baseDir: File, val name: String, branchName: String = AdeptGitRepository.MasterBranchName) extends Logging {
  import AdeptGitRepository._

  /** Add metadata and artifacts after the `commit`. Rewrites the history if needed. */
  def addMetadata(commit: Commit, variantMetata: ConfiguredVariantsMetadata, artifactMetadata: ArtifactMetadata, releaseNotes: String): AdeptCommit = {
    ???
  }

  /** Remove the metadata at `commit`. Rewrites the history if needed. */
  def removeMetadata(commit: Commit, reason: String)(func: ConfiguredVariantsMetadata => Boolean): AdeptCommit = {
    //TODO: add parameter cleanArtifact: Boolean = false
    ???
  }

  /**
   *  Search backwards in the Git history and find the first metadata
   *  matching the `func` function.
   */
  def scanFirst(func: ConfiguredVariantsMetadata => Boolean): Option[(AdeptCommit, ConfiguredVariantsMetadata)] = {
    ???
  }

  /**
   *  Search backwards in the Git history and find all metadata matching the 
   *  `func` function.
   */
  def scan(func: ConfiguredVariantsMetadata => Boolean): Seq[(AdeptCommit, ConfiguredVariantsMetadata)] = {
    ???
  }
  
  private def scanAll(stopAtFirst: Boolean)(func: ConfiguredVariantsMetadata => Boolean): Seq[(AdeptCommit, ConfiguredVariantsMetadata)] = {
    
    ???
  }

}
