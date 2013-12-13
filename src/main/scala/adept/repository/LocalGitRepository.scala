package adept.repository

import adept.core.models._
import java.io.File
import org.eclipse.jgit.transport.URIish
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.Constants
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.treewalk.filter.PathFilter
import java.io.InputStream
import java.io.InputStreamReader

object Commit {
  val Head = Commit(Constants.HEAD)
}

case class Commit(value: String) extends AnyVal {
  override def toString = value
}

class LocalGitRepository(val baseDir: File, val name: String, val commitRef: Commit) extends Repository {
  def isClean(): Boolean = {
    git.status().call().isClean()
  }

  private def getMostRecentCommit() = {
    val logIt = git.log()
      .call()
      .iterator()
    Commit(logIt.next.getId().name)
  }

  val commit: Commit = {
    try {
      if (commitRef == Commit(Constants.HEAD)) { //get actual ref of HEAD
        getMostRecentCommit()
      } else commitRef
    } catch {
      case e: org.eclipse.jgit.api.errors.NoHeadException => throw e //handle?
    }
  }
  import Repository._

  val GitPathSep = "/"

  private val fileRepository = new FileRepository(baseDir, name)

  private lazy val gitDir = repoDir
  private lazy val git = Git.open(gitDir)
  private def gitRepo = git.getRepository()

  /** Use commit() on GitRepositoryEngine instead */
  private[repository] def commit(msg: String): LocalGitRepository = {
    val revCommit = git.commit()
      .setMessage(msg)
      .call()

    new LocalGitRepository(baseDir, name, Commit(revCommit.name))
  }

  private def tree = { //TODO: make val?
    val currentCommitId = gitRepo.resolve(commit.value)

    val revWalk = new RevWalk(gitRepo)
    val revCommit = revWalk.parseCommit(currentCommitId)
    revCommit.getTree()
  }

  private def createTreeWalk(path: String) = {
    val treeWalk = new TreeWalk(gitRepo)
    treeWalk.addTree(tree)
    treeWalk.setRecursive(true)
    treeWalk.setFilter(PathFilter.create(path))
    treeWalk
  }

  /** Uses String.matches to match for variants */
  def search(regex: String): Set[Variant] = {
    val treeWalk = new TreeWalk(gitRepo)
    treeWalk.addTree(tree)
    treeWalk.setRecursive(false)

    var variants = Set.empty[Variant]

    while (treeWalk.next()) {
      if (treeWalk.isSubtree()) {
        treeWalk.enterSubtree()
      } else {
        val path = treeWalk.getPathString()
        if (path.startsWith(MetadataDirName) && path.endsWith(".json") && path.matches(regex)) {
          readBlobFile(commit, path) { reader =>
            import org.json4s.native.Serialization.read
            import adept.serialization.Formats._

            variants += read[Variant](reader)
          }
        }
      }
    }
    variants
  }

  override def hasVariant(id: Id, hash: Hash): Boolean = {
    createTreeWalk(getVariantsBlobPath(id, hash)).next()
  }

  private def gitPath(file: File): String = {
    file.getAbsolutePath.replace(gitDir.getAbsolutePath + File.separator, "")
  }

  private def add(fileWriteResult: Either[String, File]) = {
    fileWriteResult.right.map { file =>
      if (!isClean) {
        git.add()
          .addFilepattern(gitPath(file))
          .call()
      }
      file
    }
  }

  private def rm(fileDeleteResult: Either[String, File]) = {
    fileDeleteResult.right.map { file =>
      if (!isClean) {
        git.rm()
          .addFilepattern(gitPath(file))
          .call()
      }
      file
    }
  }

  def writeVariant(variant: Variant): Either[String, File] = {
    add(fileRepository.writeVariant(variant))
  }

  def deleteVariant(variant: Variant): Either[String, File] = {
    rm(fileRepository.deleteVariant(variant))
  }

  private def readBlob[A](treeWalk: TreeWalk)(f: InputStream => A) = {
    val objectId = treeWalk.getObjectId(0)
    val loader = gitRepo.open(objectId)
    try {
      Right(f(loader.openStream()))
    } finally {
      gitRepo.close()
    }
  }

  def getVariantsBasePath(id: Id): String = {
    id.value.split(IdDirSep).foldLeft(Repository.MetadataDirName) { (currentPath, currentId) =>
      currentPath + GitPathSep + currentId
    }
  }

  def getVariantsBlobPath(id: Id, hash: Hash): String = {
    getVariantsBasePath(id) + GitPathSep + hash.value + "." + Repository.JsonFileEnding
  }

  private def readBlobFile[A](commit: Commit, path: String)(f: InputStreamReader => A): Either[String, A] = {
    val treeWalk = createTreeWalk(path)

    if (!treeWalk.next()) {
      Left(s"Could not find git object for path: '$path' in '${gitDir.getAbsolutePath}' for commit: $commit")
    } else {

      readBlob(treeWalk) { is =>
        val reader = new InputStreamReader(is)
        try {
          f(reader)
        } finally {
          reader.close()
        }
      }
    }
  }

  override def readVariant(id: Id, hash: Hash): Either[String, Variant] = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._
    val path = getVariantsBlobPath(id, hash)
    readBlobFile(commit, path) { reader =>
      read[Variant](reader)
    }
  }

  override def readVariants(id: Id): Either[String, Set[Variant]] = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._

    val treeWalk = createTreeWalk(getVariantsBasePath(id))
    //imperative style makes this easier to read:
    var result: Either[String, Set[Variant]] = Right(Set.empty[Variant])
    while (treeWalk.next()) {
      val maybeVariant: Either[String, Variant] = readBlob(treeWalk) { is =>
        val reader = new InputStreamReader(is)
        try { //TODO: add validation of json
          read[Variant](reader)
        } finally {
          reader.close()
        }
      }

      (result, maybeVariant) match {
        case (Right(variants), Right(variant)) => result = Right(variants + variant)
        case (Left(errorMsg), Right(variant)) => result = Left(errorMsg)
        case (Right(variants), Left(errorMsg)) => result = Left(errorMsg)
        case (Left(errorMsg1), Left(errorMsg2)) => result = Left(errorMsg1 + "; " + errorMsg2)
      }
    }
    result
  }

  def getArtifactDescriptorPath(hash: Hash) = {
    val level1Dir = hash.value.slice(0, 4)
    val level2Dir = hash.value.slice(4, 8)
    artifactDescriptorsDir + GitPathSep + level1Dir + GitPathSep + level2Dir + GitPathSep + hash.value + "." + JsonFileEnding
  }

  override def hasArtifactDescriptor(hash: Hash): Boolean = createTreeWalk(getArtifactDescriptorPath(hash)).next()

  override def readArtifactDescriptor(hash: Hash): Either[String, Artifact] = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._
    val path = getArtifactDescriptorPath(hash)
    readBlobFile(commit, path) { reader =>
      read[Artifact](reader)
    }
  }

  override def writeArtifactDescriptor(artifact: Artifact): Either[String, File] = {
    add(fileRepository.writeArtifactDescriptor(artifact))
  }

}