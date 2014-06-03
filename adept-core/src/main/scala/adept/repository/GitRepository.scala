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
import adept.hash.Hasher
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

  override def exists: Boolean = {
    super.exists && new File(dir, ".git").isDirectory
  }

  def gc(): Unit = {
    git.gc().call() //TODO: .setProgressMonitor(progress)?
  }

  def hasCommit(commit: Commit): Boolean = {
    exists && usingRevWalk { (gitRepo, revWalk) =>
      lookup(gitRepo, revWalk, commit.value).isDefined
    }
  }

  def getRemoteUri(remoteName: String): Option[String] = { //TODO: rename to getLocation(s)?
    if (remoteName != GitRepository.DefaultRemote) throw new Exception("Cannot get " + remoteName + " remote uri because we only support: " + DefaultRemote + ".") //TODO: support other names
    val repoConfig = git.getRepository().getConfig()
    Option(repoConfig.getString(
      ConfigConstants.CONFIG_REMOTE_SECTION, remoteName,
      ConfigConstants.CONFIG_KEY_URL))
  }

  def addRemoteUri(remoteName: String, uri: String) = {
    if (remoteName != GitRepository.DefaultRemote)
      throw new Exception("Cannot get " + remoteName + " remote uri because we only support: " + DefaultRemote + ".") //TODO: support other names
    val repoConfig = git.getRepository().getConfig()
    repoConfig.setString(ConfigConstants.CONFIG_REMOTE_SECTION, GitRepository.DefaultRemote, ConfigConstants.CONFIG_KEY_URL, uri)
    repoConfig.setString(ConfigConstants.CONFIG_REMOTE_SECTION, GitRepository.DefaultRemote, "fetch", "+refs/heads/*:refs/remotes/origin/*")
    repoConfig.save()
  }

  def push(passphrase: Option[String] = None, progress: ProgressMonitor = NullProgressMonitor.INSTANCE) = {
    GitHelpers.withGitSshCredentials(passphrase) {
      git.push()
        .setProgressMonitor(progress)
        .call()
    }
  }

  def pull(remoteName: String, branch: String, passphrase: Option[String] = None, progress: ProgressMonitor = NullProgressMonitor.INSTANCE) = {
    GitHelpers.withGitSshCredentials(passphrase) {
      val repoConfig = git.getRepository().getConfig() //
      repoConfig.setString(ConfigConstants.CONFIG_BRANCH_SECTION, GitRepository.DefaultBranchName, "remote", GitRepository.DefaultRemote)
      repoConfig.setString(ConfigConstants.CONFIG_BRANCH_SECTION, GitRepository.DefaultBranchName, "merge", "refs/heads/" + GitRepository.DefaultBranchName)
      repoConfig.save()
      val pullResults = git.pull().setProgressMonitor(progress).call()
      if (pullResults.isSuccessful())
        getHead //TODO: is this right?
      else {
        throw new Exception("Could not pull successfully in: " + dir.getAbsolutePath + " (and we do not support this yet): " + pullResults) //TODO: <--
      }
    }
  }

  def clone(uri: String, passphrase: Option[String] = None, progress: ProgressMonitor = NullProgressMonitor.INSTANCE) = {
    GitHelpers.withGitSshCredentials(passphrase) {
      Git.cloneRepository()
        .setURI(uri)
        .setDirectory(dir)
        .setProgressMonitor(progress)
        .call()
    }
  }

  def checkout(branch: String) = { //TODO: REMOVE this one and manage remote uris properly
    git.checkout().setName(branch).call()
  }

  def getHead: Commit = usingRevWalk { (gitRepo, revWalk) =>
    val resolvedRef = gitRepo.resolve(Head)
    if (resolvedRef != null) {
      Commit(revWalk.lookupCommit(resolvedRef).name)
    } else {
      throw new Exception("In Git: " + gitRepo.getDirectory().getAbsolutePath() + ": cannot resolve commit: " + Head)
    }
  }

  def add(files: Set[File]): Set[File] = synchronized { //this is synchronized because git locks when it writes and we do not want to try to break that one, feels bad but that is the way it is
    add(files.toSeq: _*)
  }

  def add(files: File*): Set[File] = synchronized { //this is synchronized because git locks when it writes and we do not want to try to break that one, feels bad but that is the way it is
    files.foreach { file =>
      git.add().addFilepattern(asGitPath(file)).call()
    }
    files.toSet
  }

  def rm(files: Set[File]): Set[File] = synchronized { //this is synchronized because git locks when it writes and we do not want to try to break that one, feels bad but that is the way it is
    rm(files.toSeq: _*)
  }

  def rm(files: File*): Set[File] = synchronized { //this is synchronized because git locks when it writes and we do not want to try to break that one, feels bad but that is the way it is
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
    import collection.JavaConverters._
    //    println(name + " status: " + git.status.call().getModified().asScala)
    git.status().call().isClean()
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

  private[repository] def usingInfoInputStream[A](id: Id, hash: VariantHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getInfoFile(id, hash)))(block)
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
        block(Left("Could not create git tree for commit: " + commit + " for dir: " + dir.getAbsolutePath))
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
