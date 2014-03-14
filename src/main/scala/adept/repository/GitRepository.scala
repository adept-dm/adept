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

/**
 *  Defines Git operations and defines streams methods used for READ operations.
 *
 *  See [[adept.repository.Repository]] for WRITE operations and layout
 */
class GitRepository(override val baseDir: File, override val name: RepositoryName, progress: ProgressMonitor = NullProgressMonitor.INSTANCE, val passphrase: Option[String] = None) extends Repository(baseDir, name) with Logging {
  import GitRepository._
  import Repository._

  def hasCommit(commit: Commit): Boolean = {
    usingRevWalk { (gitRepo, revWalk) =>
      lookup(gitRepo, revWalk, commit.value).isDefined
    }
  }

  def fetchRemote(uri: String) = {
    GitHelpers.withGitSshCredentials(passphrase) {
      git.fetch().setRemote(uri).setProgressMonitor(progress).call()
    }
  }

  def checkout(branch: String) = { //TODO: REMOVE this one and manage remote uris properly
    git.checkout().setName(branch).call()
  }

  private def git = Git.open(dir)

  private[repository] def usingGitRepo[A](func: JGitRepository => A): A = {
    var repo: JGitRepository = null
    try {
      repo = git.getRepository()
      func(repo)
    } finally {
      if (repo != null) repo.close()
    }
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
    val initCommit = commit("Initialized " + name.value)
    git.tag().setName(InitTag).call()
    initCommit
  }

  def commit(msg: String): Commit = {
    val revCommit = git.commit().setMessage(msg).call
    Commit(revCommit.name)
  }

  def pull(remoteName: String, uri: String) = {
    ???
  }

  def push(remoteName: String, uri: String) = {
    ???
  }

  private def readBlob[A](treeWalk: TreeWalk, gitRepo: JGitRepository)(f: InputStream => A) = {
    val objectId = treeWalk.getObjectId(0) //nth == 0, means we are reading the 0th tree
    val loader = gitRepo.open(objectId)
    val stream = loader.openStream()
    try {
      f(loader.openStream())
    } finally {
      stream.close()
    }
  }

  private[adept] def asGitPath(file: File): String = {
    file.getAbsolutePath().replace(dir.getAbsolutePath() + File.separator, "").replace(File.separator, GitRepository.GitPathSep)
  }

  private def lookup(gitRepo: JGitRepository, revWalk: RevWalk, commitString: String) = {
    val resolvedRef = gitRepo.resolve(commitString)
    if (resolvedRef != null) {
      val revCommit = revWalk.lookupCommit(resolvedRef)
      Option(revCommit)
    } else {
      None
    }
  }

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

  def usingResolutionResultsInputStream[A](id: Id, hash: VariantHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getResolutionResultsFile(id, hash)))(block)
  }

  def usingVariantInputStream[A](id: Id, hash: VariantHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getVariantFile(id, hash)))(block)
  }

  def usingArtifactInputStream[A](hash: ArtifactHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getArtifactFile(hash)))(block)
  }

  def usingOrderInputStream[A](id: Id, orderId: OrderId, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getOrderFile(id, orderId)))(block)
  }

  def isClean: Boolean = {
    git.status().call().isClean()
  }

  private def usePath[A](path: Option[String], commit: Commit)(accumulate: String => Option[A]): Set[A] = {
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
      }
      accumulator
    }
  }

  def listVariants(id: Id, commit: Commit): Set[VariantHash] = {
    val HashExtractionRegex = s"""$VariantsMetadataDirName$GitPathSep${id.value}$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep$VariantMetadataFileName""".r
    usePath[VariantHash](Some(VariantsMetadataDirName), commit) { path =>
      path match {
        case HashExtractionRegex(level1, level2, level3) =>
          Some(VariantHash(level1 + level2 + level3))
        case _ => None
      }
    }
  }

  /** get N order ids from start (could be size of active order ids)  */
  def getXOrderId(id: Id, start: Int = 0, N: Int = 1): Set[OrderId] = {
    if (N < 1) throw new IllegalArgumentException("Cannot get " + N + " new order ids (n is smaller than 1)")
    if (N < (start + 1)) throw new IllegalArgumentException("Cannot an empty order set " + N + " new order ids (n is smaller than start:" + start + ")")
    if (start < 0) throw new IllegalArgumentException("Cannot start at " + start + " new order ids (start is smaller than 0)")

    var seed = id.value + {
      usingRevWalk { (gitRepo, revWalk) =>
        val revCommit = lookup(gitRepo, revWalk, InitTag)
          .getOrElse(throw new Exception("Cannot get next order because init tag: " + InitTag + " does not resolve a commit - is the repository " + dir.getAbsolutePath + "not properly initialized?"))
        revCommit.name
      }
    }

    def createHash(lastSeed: String) = {
      Hasher.hash((lastSeed * 2 + 42).getBytes)
    }

    for (i <- 0 to start) {
      val lastSeed = seed
      seed = createHash(lastSeed)
    }
    (for (i <- 0 to (N - start - 1)) yield {
      val lastSeed = seed
      seed = createHash(lastSeed)
      OrderId(seed)
    }).toSet
  }

  def listIds(commit: Commit): Set[Id] = {
    val IdExtractionRegex = s"""$VariantsMetadataDirName$GitPathSep(.*)$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep$VariantMetadataFileName""".r
    usePath[Id](Some(VariantsMetadataDirName), commit) { path =>
      path match {
        case IdExtractionRegex(id, level1, level2, level3) =>
          Some(Id(id))
        case _ => None
      }
    }
  }

  def listActiveOrderIds(id: Id, commit: Commit): Set[OrderId] = {
    val orderPath = s"""$VariantsMetadataDirName$GitPathSep${id.value}"""
    val OrderIdExtractionRegEx = s"""$orderPath$GitPathSep$OrderFileNamePrefix(.*?)""".r
    val orderIds = usePath[OrderId](Some(orderPath), commit) { path =>
      path match {
        case OrderIdExtractionRegEx(id) => Some(OrderId(id))
        case _ => None
      }
    }
    orderIds
  }
}

object GitRepository {
  val InitTag = "init"
  val DefaultBranchName = "master"
  val Head = Constants.HEAD

  val GitPathSep = "/" //the character that separates paths in Git

  def clone(baseDir: File, remoteName: String, uri: String) = {
    ???
  }
}