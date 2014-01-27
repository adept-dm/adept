package adept.repository

import java.io.File
import adept.repository.models._
import adept.logging.Logging
import org.eclipse.jgit.lib.Constants
import org.eclipse.jgit.lib.{ Repository => JGitRepository }
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.revwalk.RevCommit
import java.io.RandomAccessFile
import java.nio.channels.FileLock

case class WriteLockException(repo: AdeptGitRepository) extends Exception("Could not lock: " + repo.dir.getAbsolutePath)
case class InitException(repo: AdeptGitRepository, reason: String) extends Exception("Could not initialize '" + repo.dir.getAbsolutePath + "': " + reason)

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

  val GitPathSep = "/" //the character that separates paths in Git
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
class AdeptGitRepository(val baseDir: File, val name: String) extends Logging {
  import AdeptGitRepository._

  //FIXME: Allow branch as a class parameter/field?
  final val branchName: String = AdeptGitRepository.MasterBranchName

  val dir = getRepoDir(baseDir, name)

  private def init(git: Git) = {
    git.commit().setMessage("Initialized " + name).call()
    git.tag().setName(InitTag).call()
  }

  private lazy val git = usingWriteLock {
    if (dir.isDirectory()) {
      val git = Git.open(dir)
      val repo = git.getRepository()
      try {
        if (!repoIsInitialized(repo)) {
          throw InitException(this, "Directory exists but repository is not initialized.")
        }
      } finally {
        repo.close()
      }
      git
    } else if (!dir.isDirectory() && dir.mkdirs()) {
      val git = Git.init().setDirectory(dir).call()
      init(git)
      git
    } else {
      throw InitException(this, "Could not make directories while initializing.")
    }
  }

  private def usingGitRepo[A](func: JGitRepository => A): A = {
    var repo: JGitRepository = null
    try {
      repo = git.getRepository()
      func(repo)
    } finally {
      if (repo != null) repo.close()
    }
  }

  def isClean: Boolean = {
    git.status().call().isClean()
  }

  private def repoIsInitialized(repo: JGitRepository) = {
    repo.getTags().containsKey(InitTag)
  }

  def isInitialized: Boolean = {
    if (dir.isDirectory()) {
      usingGitRepo { repo =>
        repoIsInitialized(repo)
      }
    } else false
  }

  def getMostRecentCommit: AdeptCommit= usingGitRepo { gitRepo =>
    usingRevWalk(gitRepo) { revWalk =>
      new AdeptCommit(this, revWalk.lookupCommit(gitRepo.resolve(InitTag)))
    }
  }

  def lockFile = new File(baseDir, "." + name + ".lock")

  private def usingWriteLock[A](func: => A): A = synchronized {
    var lock: FileLock = null
    //java.nio.channels.OverlappingFileLockException
    try {
      val channel = new RandomAccessFile(lockFile, "rw").getChannel();
      lock = channel.tryLock()
      if (lock == null) throw WriteLockException(this)
      else {
        func
      }
    } finally {
      if (lock != null) {
        lock.release()
      }
    }
  }

  private implicit def revCommitToCommit(revCommit: RevCommit): Commit = {
    Commit(revCommit.name)
  }

  private def usingRevWalk[A](gitRepo: JGitRepository)(func: RevWalk => A) = {
    val revWalk = new RevWalk(gitRepo)
    try {
      func(revWalk)
    } finally {
      revWalk.dispose()
    }
  }

  private def lookup(gitRepo: JGitRepository, string: String): AdeptCommit = {
    usingRevWalk(gitRepo) { revWalk =>
      val revCommit = revWalk.lookupCommit(gitRepo.resolve(InitTag))
      new AdeptCommit(this, revCommit)
    }
  }

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
