package adept.repository

import java.io.File
import adept.repository.models._
import java.io.InputStream
import adept.resolution.models.Id
import adept.artifact.models.ArtifactHash
import org.eclipse.jgit.lib.{ Constants, Repository => JGitRepository }
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.treewalk.filter.PathFilter
import org.eclipse.jgit.lib.ProgressMonitor
import org.eclipse.jgit.lib.NullProgressMonitor
import java.io.FilenameFilter
import adept.logging.Logging
import adept.utils.Hasher
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.revwalk.filter.RevFilter
import org.eclipse.jgit.lib.ConfigConstants

/**
 *  Defines Git operations and streams methods used for READ operations.
 *
 *  See [[adept.repository.Repository]] for WRITE operations and layout
 */
class GitRepository(override val baseDir: File, override val name: RepositoryName) extends Repository(baseDir, name) with Logging {
  import GitRepository._
  import Repository._

  def exists: Boolean = {
    new File(dir, ".git").isDirectory
  }

  def gc(): Unit = {
    git.gc().call() //TODO: .setProgressMonitor(progress)?
  }

  def hasCommit(commit: Commit): Boolean = {
    usingRevWalk { (gitRepo, revWalk) =>
      lookup(gitRepo, revWalk, commit.value).isDefined
    }
  }

  def getRemoteUri(remoteName: String): Option[String] = {
    if (remoteName != GitRepository.DefaultRemote) throw new Exception("Cannot get " + remoteName + " remote uri because we only support: " + DefaultRemote + ".") //TODO: support other names
    val repoConfig = git.getRepository().getConfig()
    Some(repoConfig.getString(
      ConfigConstants.CONFIG_REMOTE_SECTION, remoteName,
      ConfigConstants.CONFIG_KEY_URL))
  }

  def fetchRemote(uri: String, passphrase: Option[String], progress: ProgressMonitor = NullProgressMonitor.INSTANCE) = {
    GitHelpers.withGitSshCredentials(passphrase) {
      git.fetch().setRemote(uri).setProgressMonitor(progress).call()
    }
  }

  def checkout(branch: String) = { //TODO: REMOVE this one and manage remote uris properly
    git.checkout().setName(branch).call()
  }

  def getHead: Commit = usingRevWalk { (gitRepo, revWalk) =>
    val resolvedRef = gitRepo.resolve(Head)
    Commit(revWalk.lookupCommit(resolvedRef).name)
  }

  def add(files: Set[File]): Set[File] = {
    add(files.toSeq: _*)
  }

  def add(files: File*): Set[File] = {
    files.foreach { file =>
      git.add().addFilepattern(asGitPath(file)).call()
    }
    files.toSet
  }

  def rm(files: Set[File]): Set[File] = {
    rm(files.toSeq: _*)
  }

  def rm(files: File*): Set[File] = {
    files.foreach { file =>
      git.rm().addFilepattern(asGitPath(file)).call()
    }
    files.toSet
  }

  def init() = {
    val git = Git.init().setDirectory(dir).call()
    val revCommit = git.commit().setMessage("Initialized " + name.value).call
    val initCommit = Commit(revCommit.name)
    git.tag().setName(InitTag).call()
    initCommit
  }

  def isClean: Boolean = {
    git.status().call().isClean()
  }

  def update(remoteName: String, uri: String) = {
    ???
  }

  def publish(remoteName: String, uri: String) = {
    ???
  }

  def commit(msg: String): Commit = {
    if (isClean) {
      logger.debug("Tried to commit to empty repository, ignoring... Message was: " + msg)
      getHead
    } else {
      val revCommit = git.commit().setMessage(msg).call
      Commit(revCommit.name)
    }
  }

  def asGitPath(file: File): String = {
    file.getAbsolutePath().replace(dir.getAbsolutePath() + File.separator, "").replace(File.separator, GitRepository.GitPathSep)
  }

  //Members private to repository:
  private[repository] def git = Git.open(dir)

  private[repository] def usingGitRepo[A](func: JGitRepository => A): A = {
    var repo: JGitRepository = null
    try {
      repo = git.getRepository()
      func(repo)
    } finally {
      if (repo != null) repo.close()
    }
  }

  private[repository] def usingRevWalk[A](func: (JGitRepository, RevWalk) => A) = {
    usingGitRepo { gitRepo =>
      val revWalk = new RevWalk(gitRepo)
      try {
        func(gitRepo, revWalk)
      } finally {
        revWalk.release()
      }
    }
  }

  private[repository] def usingTreeWalk[A](func: (JGitRepository, RevWalk, TreeWalk) => A) = {
    usingRevWalk { (gitRepo, revWalk) =>
      val treeWalk = new TreeWalk(gitRepo)
      try {
        func(gitRepo, revWalk, treeWalk)
      } finally {
        treeWalk.release()
      }
    }
  }

  private[repository] def lookup(gitRepo: JGitRepository, revWalk: RevWalk, commitString: String) = {
    val resolvedRef = gitRepo.resolve(commitString)
    if (resolvedRef != null) {
      val revCommit = revWalk.lookupCommit(resolvedRef)
      Option(revCommit)
    } else {
      None
    }
  }

  private[repository] def readBlob[A](treeWalk: TreeWalk, gitRepo: JGitRepository)(f: InputStream => A) = {
    val objectId = treeWalk.getObjectId(0) //nth == 0, means we are reading the 0th tree
    val loader = gitRepo.open(objectId)
    val stream = loader.openStream()
    try {
      f(loader.openStream())
    } finally {
      stream.close()
    }
  }

  /** @returns first commit found (if any) in _1 and second in _2 */
  private[repository] def compareCommits(thisCommit: Commit, thatCommit: Commit): (Option[Commit], Option[Commit]) = {
    def equalCommits(commit: Commit, revCommit: RevCommit): Boolean = {
      commit.value == revCommit.name
    }

    def checkMatch(current: RevCommit, thisCommit: Commit, thatCommit: Commit): Option[Commit] = {
      if (equalCommits(thisCommit, current)) {
        Some(thisCommit)
      } else if (equalCommits(thatCommit, current)) {
        Some(thatCommit)
      } else None
    }

    usingRevWalk { (gitRepo, revWalk) =>
      try {
        val start = lookup(gitRepo, revWalk, Head).getOrElse(throw new Exception("Cannot find " + Head + " in git repo: " + dir.getAbsolutePath))
        revWalk.markStart(start)
        revWalk.setRevFilter(RevFilter.NO_MERGES)
        val it = revWalk.iterator()

        var first: Option[Commit] = None
        var second: Option[Commit] = None

        while (it.hasNext && (first.isEmpty || second.isEmpty)) {
          val current = it.next()
          if (first.isEmpty) {
            first = checkMatch(current, thisCommit, thatCommit)
            if (first.nonEmpty && thisCommit == thatCommit) {
              second = first
            }
          } else if (first.nonEmpty && second.isEmpty) {
            second = checkMatch(current, thisCommit, thatCommit)
          }
        }
        first -> second
      }
    }
  }

  private[repository] def usingResolutionResultsInputStream[A](id: Id, hash: VariantHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getResolutionResultsFile(id, hash)))(block)
  }

  private[repository] def usingVariantInputStream[A](id: Id, hash: VariantHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getVariantFile(id, hash)))(block)
  }

  private[repository] def usingArtifactInputStream[A](hash: ArtifactHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getArtifactFile(hash)))(block)
  }

  private[repository] def usingRankingInputStream[A](id: Id, rankId: RankId, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getRankingFile(id, rankId)))(block)
  }

  private[repository] def usingRepositoryLocationsStream[A](name: RepositoryName, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getRepositoryLocationsFile(name)))(block)
  }

  private[repository] def usePath[A](path: Option[String], commit: Commit)(accumulate: String => Option[A]): Set[A] = {
    usingTreeWalk { (gitRepo, revWalk, treeWalk) =>
      var accumulator = Set.empty[A]
      val revCommit = lookup(gitRepo, revWalk, commit.value).getOrElse {
        throw new Exception("Could not find: " + commit + " in " + dir.getAbsolutePath)
      }
      try {
        revWalk.markStart(revCommit)
      } catch {
        case e: org.eclipse.jgit.errors.MissingObjectException =>
          throw new Exception("Could not mark commit: " + revCommit + " in " + dir.getAbsolutePath, e)
      }
      val currentTree = revCommit.getTree()
      if (currentTree != null) {
        treeWalk.addTree(currentTree)
        path.foreach(p => treeWalk.setFilter(PathFilter.create(p)))
        treeWalk.setRecursive(true) //without recursive Git will return the directory, not the file
        while (treeWalk.next()) {
          if (!treeWalk.isSubtree()) {
            accumulator ++= accumulate(treeWalk.getPathString())
          }
        }
      } else {
        logger.warn("Could not find get tree: " + revCommit + " for path: " + path + " in " + commit.value)
      }
      accumulator
    }
  }

  //Private members

  private def usingInputStream[A](commit: Commit, path: String)(block: Either[String, Option[InputStream]] => A): A = {
    usingTreeWalk { (gitRepo, revWalk, treeWalk) =>
      val revCommit = lookup(gitRepo, revWalk, commit.value).getOrElse {
        throw new Exception("Could not find: " + commit + " in " + dir.getAbsolutePath)
      }
      try {
        revWalk.markStart(revCommit)
      } catch {
        case _: org.eclipse.jgit.errors.MissingObjectException =>
          block(Left("Cannot find commit: " + commit + " in " + dir))
      }
      val currentTree = revCommit.getTree()
      if (currentTree != null) { //if null means we on an empty commit (no tree)
        treeWalk.addTree(currentTree)
        treeWalk.setFilter(PathFilter.create(path))
        treeWalk.setRecursive(true) //without recursive Git will return the directory, not the file
        val res = if (treeWalk.next()) {
          readBlob(treeWalk, gitRepo) { is =>
            block(Right(Some(is)))
          }
        } else {
          block(Right(None))
        }
        if (treeWalk.next()) throw new Exception("Found too many files matching path: " + path + " this one was: " + treeWalk.getPathString)
        else res
      } else {
        block(Left("Could not find file for path: " + path + " in commit: " + commit + " for dir: " + dir.getAbsolutePath))
      }
    }
  }
}

object GitRepository {
  val InitTag = "init"
  val DefaultRemote = "origin"
  val DefaultBranchName = "master"
  val Head = Constants.HEAD

  val GitPathSep = "/" //the character that separates paths in Git
}