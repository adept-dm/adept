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

  def add(files: Set[File]): Unit = {
    add(files.toSeq: _*)
  }

  def add(files: File*): Unit = {
    files.foreach { file =>
      git.add().addFilepattern(asGitPath(file)).call()
    }
  }

  def init() = {
    val git = Git.init().setDirectory(dir).call()
    commit("Initialized " + name.value)
    Commit(git.tag().setName(InitTag).call().getName())
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

  private def asGitPath(file: File): String = {
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

  def usingRepositoryInputStream[A](id: Id, hash: VariantHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getRepositoryFile(id, hash)))(block)
  }

  def usingVariantInputStream[A](id: Id, hash: VariantHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getVariantFile(id, hash)))(block)
  }

  def usingArtifactInputStream[A](hash: ArtifactHash, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getArtifactFile(hash)))(block)
  }

  def usingOrderLookupInputStream[A](id: Id, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getOrderLookupFile(id)))(block)
  }

  def usingOrderInputStream[A](id: Id, orderId: OrderId, commit: Commit)(block: Either[String, Option[InputStream]] => A): A = {
    usingInputStream(commit, asGitPath(getOrderFile(id, orderId)))(block)
  }

  private def useVariantsPath[A](commit: Commit)(accumulate: String => Option[A]): Set[A] = {
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
        treeWalk.setFilter(PathFilter.create(VariantsMetadataDirName))
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
    val HashExtractionExpression = s"""$VariantsMetadataDirName$GitPathSep${id.value}$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep$VariantMetadataFileName""".r
    useVariantsPath[VariantHash](commit) { path =>
      path match {
        case HashExtractionExpression(level1, level2, level3) =>
          Some(VariantHash(level1 + level2 + level3))
        case _ => None
      }
    }
  }

  private lazy val OrderFileFilter = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = {
      name.startsWith(OrderFileNamePrefix)
    }
  }

  private lazy val OrderFileIdRegEx = s"""$OrderFileNamePrefix(\\d+)""".r

  def getNextOrderId(id: Id, commit: Commit): OrderId = {
    logger.warn("Next order Id might fail, because it is not reading git") //TODO: use commit!
    if (commit != getHead || (commit != getHead && commit.value.toUpperCase != "HEAD")) {
      logger.error("Cannot get next order on: " + commit + " - it is not implemented yet")
      ???
    }
    val orderDir = getIdFile(variantsMetadataDir, id)
    val orderFiles = listFiles(orderDir, OrderFileFilter).map(_.getName)
    val nextId = orderFiles.sorted.lastOption match {
      case Some(OrderFileIdRegEx(id)) => id.toInt + 1 //toInt is 'safe' because of regex 
      case _ => 1
    }
    OrderId(nextId)
  }

  private def listFiles(dir: File, filter: FilenameFilter): Seq[File] = { //TODO: remove this shouuld not be used!
    val files = dir.listFiles(OrderFileFilter)
    if (files != null) {
      files.toSeq
    } else Seq.empty
  }

  def listIds(commit: Commit): Set[Id] = {
    val IdExtractionExpression = s"""$VariantsMetadataDirName$GitPathSep(.*)$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep(.*?)$GitPathSep$VariantMetadataFileName""".r
    useVariantsPath[Id](commit) { path =>
      path match {
        case IdExtractionExpression(id, level1, level2, level3) =>
          Some(Id(id))
        case _ => None
      }
    }
  }

 def listActiveOrderIds(id: Id, commit: Commit): Set[OrderId] = {
    usingOrderLookupInputStream(id, commit) {
      case Right(Some(is)) =>
        io.Source.fromInputStream(is).getLines.map { line =>
          OrderId(line.trim().toInt)
        }.toSet
      case Right(None) => Set.empty
      case Left(error) => throw new Exception("Could not read order file for: " + id + " in " + dir.getAbsolutePath + " for: " + commit )
    }
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