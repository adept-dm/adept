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
import java.io.FileWriter
import adept.models._
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.treewalk.filter.PathFilter
import java.io.InputStream
import java.io.InputStreamReader

case class WriteLockException(repo: AdeptGitRepository, reason: String) extends Exception("Could not lock '" + repo.dir.getAbsolutePath + "': " + reason)
case class InitException(repo: AdeptGitRepository, reason: String) extends Exception("Could not initialize '" + repo.dir.getAbsolutePath + "': " + reason)
case class UpdateMetadataException(repo: AdeptGitRepository, reason: String) extends Exception("Could not update metadata in '" + repo.dir.getAbsolutePath + "': " + reason)
case class FileNotFoundException(repo: AdeptGitRepository, file: File) extends Exception("File '" + file.getAbsolutePath() + "' is not git directory: '" + repo.dir.getAbsolutePath + "'.")

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

  private[repository] val git = usingWriteLock {
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

  private[repository] def usingGitRepo[A](func: JGitRepository => A): A = {
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

  def getMostRecentCommit: AdeptCommit = usingRevWalk { (gitRepo, revWalk) =>
    new AdeptCommit(this, revWalk.lookupCommit(gitRepo.resolve(InitTag)))
  }

  def lockFile = new File(baseDir, "." + name + ".lock")

  @volatile private var locked = false

  private def usingWriteLock[A](func: => A): A = synchronized {
    var lock: FileLock = null
    try {
      val channel = new RandomAccessFile(lockFile, "rw").getChannel();
      lock = channel.tryLock() //TODO: re-throw: java.nio.channels.OverlappingFileLockException
      if (lock == null) throw WriteLockException(this, "Could not acquire lock: " + lockFile + ".")
      else {
        locked = true
        func
      }
    } finally {
      if (lock != null) {
        lock.release()
        lockFile.delete()
        locked = false
      }
    }
  }

  private def requiringLock[A](func: => A): A = {
    if (locked == false) throw WriteLockException(this, "Required lock, but it was not set.")
    func
  }

  private implicit def revCommitToCommit(revCommit: RevCommit): Commit = {
    Commit(revCommit.name)
  }

  private def usingRevWalk[A](func: (JGitRepository, RevWalk) => A) = {
    usingGitRepo { gitRepo =>
      val revWalk = new RevWalk(gitRepo)
      try {
        func(gitRepo, revWalk)
      } finally {
        revWalk.release()
      }
    }
  }

  private def usingTreeWalk[A](func: (JGitRepository, RevWalk, TreeWalk) => A) = {
    usingRevWalk { (gitRepo, revWalk) =>

      val treeWalk = new TreeWalk(gitRepo)
      try {
        func(gitRepo, revWalk, treeWalk)
      } finally {
        treeWalk.release()
      }
    }

  }

  def getVariantsMetadataDir(id: Id): File = requiringLock {
    val dir = id.value.split(IdDirSep).foldLeft(getVariantsDir(baseDir, name)) { (currentPath, dir) =>
      new File(currentPath, dir)
    }
    if (!(dir.isDirectory() || dir.mkdirs())) throw InitException(this, "Could not create variants metadata dir: " + dir.getAbsolutePath)
    dir
  }

  def getVariantsMetadataFile(id: Id, hash: Hash): File = {
    new File(getVariantsMetadataDir(id), hash.value + "." + JsonFileEnding)
  }

  private def gitPath(file: File): String = {
    if (file.getAbsolutePath().startsWith(dir.getAbsolutePath)) {
      file.getAbsolutePath.replace(dir.getAbsolutePath + File.separator, "")
    } else {
      throw FileNotFoundException(this, file)
    }
  }

  private def readBlob[A](treeWalk: TreeWalk, gitRepo: JGitRepository)(f: InputStream => A) = {
    val objectId = treeWalk.getObjectId(0) //nth == 0, means we are reading the 0th tree
    val loader = gitRepo.open(objectId)
    val stream = loader.openStream()
    try {
      Right(f(loader.openStream()))
    } finally {
      stream.close()
    }
  }

  private def lookup(gitRepo: JGitRepository, revWalk: RevWalk, commit: Commit) = revWalk.lookupCommit(gitRepo.resolve(commit.value))

  //TODO: optimize to only look in certain paths?
  private[adept] def listContent(commit: Commit): MetadataContent = requiringLock {
    var configuredVariantsMetadata = Set.empty[ConfiguredVariantsMetadata]

    usingTreeWalk { (gitRepo, revWalk, treeWalk) =>
      val containingDir = VariantsDirName
      val revCommit = lookup(gitRepo, revWalk, commit)
      revWalk.markStart(revCommit)
      val currentTree = revCommit.getTree()
      if (currentTree != null) { //if null means we on an empty commit (no tree)
        treeWalk.addTree(currentTree)
        treeWalk.setRecursive(true)
        treeWalk.setFilter(PathFilter.create(containingDir))

        while (treeWalk.next()) {
          val currentPath = treeWalk.getPathString
          if (treeWalk.isSubtree()) {
            treeWalk.enterSubtree()
          } else if (currentPath.startsWith(containingDir) && currentPath.endsWith(JsonFileEnding)) { //TODO: more verifications?
            readBlob(treeWalk, gitRepo) { is =>
              val reader = new InputStreamReader(is)
              try {
                configuredVariantsMetadata += ConfiguredVariantsMetadata.fromJson(reader)
              } finally {
                reader.close()
              }
            }
          }
        }
      } else {
        logger.debug("Skipped empty commit: " + commit + " in " + dir)
      }
    }
    MetadataContent(configuredVariantsMetadata, Set.empty)
  }

  /**
   *  Update metadata and artifacts after the `commit`. Rewrites the history if needed.
   *
   *  `removals` is a function that is based on the content of the current commit, returns some (old) files to be removed.
   *  `additions` does the inverse (i.e. adds new files)
   */
  def updateMetadata(removals: MetadataContent => Seq[File], additions: MetadataContent => Seq[File], commitMsg: String, commit: Commit = Commit(Head)): AdeptCommit = usingWriteLock {
    val mostRecentCommit = getMostRecentCommit
    if (!isClean) throw UpdateMetadataException(this, "Directory is not clean")
    else if (commit == mostRecentCommit.commit || commit.value == Head) { //we are on Head of our branch, nothing funky required

      removals(listContent(commit)).foreach { file =>
        git.rm().addFilepattern(gitPath(file)).call()
      }

      additions(listContent(commit)).foreach { file =>
        git.add().addFilepattern(gitPath(file)).call()
      }

      val status = git.status.call()
      val changed = {
        import collection.JavaConversions._
        val conflicting = status.getConflicting()
        if (conflicting.nonEmpty) throw new UpdateMetadataException(this, "Found conflicting files: " + conflicting.toList)
        else {
          status.getAdded() ++ status.getChanged() ++ status.getRemoved()
        }
      }

      if (changed.nonEmpty) {
        new AdeptCommit(this, git.commit().setMessage(commitMsg).call())
      } else {
        mostRecentCommit
      }
    } else { //we must wedge data in between some existing commits
      ???
    }
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
