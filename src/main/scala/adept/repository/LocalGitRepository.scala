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
import org.eclipse.jgit.merge.MergeStrategy
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.revwalk.filter.RevFilter

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
  private def gitRepo = git.getRepository() //TODO: withGitRepo[A](f: org.eclipse.jgit.Repository => A) = {val gitRepo = git.getRepository; try { f(gitRepo) } finally { gitRepo.close() } }   

  /** Use commit() on GitRepositoryEngine instead */
  private[repository] def commit(msg: String): LocalGitRepository = {
    val revCommit = git.commit()
      .setMessage(msg)
      .call()
    new LocalGitRepository(baseDir, name, Commit(revCommit.name))
  }

  //TODO: fix LocalGitRepository! this method doesn't make sense to have here, because now localgitrepo is bound to one commit. think through how it should work
  private[adept] def scan(id: Id)(predicate: Variant => Boolean): Option[Commit] = {
    val revWalk = new RevWalk(gitRepo)
    revWalk.markStart(revWalk.lookupCommit(gitRepo.resolve(Constants.HEAD)))

    //skip merges
    revWalk.setRevFilter(RevFilter.NO_MERGES)
    val it = revWalk.iterator()
    var matchingCommit: Option[Commit] = None

    while (it.hasNext && matchingCommit.isEmpty) {
      val revCommit = it.next()
      val tree = revCommit.getTree()
      val treeWalk = new TreeWalk(gitRepo)
      treeWalk.addTree(tree)
      treeWalk.setRecursive(false)
      treeWalk.setFilter(PathFilter.create(getVariantsBasePath(id)))

      while (treeWalk.next() && matchingCommit.isEmpty) {
        if (treeWalk.isSubtree()) {
          treeWalk.enterSubtree()
        } else {
          readBlob(treeWalk) { is =>
            val reader = new InputStreamReader(is)
            try {
              import org.json4s.native.Serialization.read
              import adept.serialization.Formats._
              if (predicate(read[Variant](reader))) {
                matchingCommit = Some(Commit(revCommit.name))
              }
            } finally {
              reader.close()
            }
          }
        }
      }
    }
    matchingCommit
  }

  /** wedge (using merge) a variant in after a commit - will keep the current commits */
  private[adept] def wedge(variant: Variant, commit: Commit) = {
    //TODO: use constants instead of strings!
    val branchName = variant.attribute("version").values.mkString(" ")
    println(branchName)
    git.branchCreate().setStartPoint(commit.value).setName(branchName).call()
    
    val previousVariants = readVariants(variant.id).right.get //TODO: fix right...
    git.checkout().setName(branchName).call()
    val wedgedCommit = {
      //TODO: fix this piece of code. readVariants is too heavy and unnecessary. should be able to support more than one variant. rm is unsafe. the list goes on...
      if (previousVariants.size > 1) throw new Exception("Wedge with more than one variant is not supported yet")
      git.rm().addFilepattern(getVariantsBasePath(variant.id)).call()
      writeVariant(variant)
      git.commit()
        .setMessage(variant.attribute("version").values.mkString(" "))
        .call()
    }

    git.checkout().setName("master").call()
    git.merge().setStrategy(MergeStrategy.OURS).include(wedgedCommit).setCommit(false).call()

    previousVariants.foreach { variant =>
      deleteVariant(variant)
    }

    val mergeCommit = git.commit().setMessage("Wedged " + variant + " after " + commit.value).call()
    val masterRef = git.checkout().setName("master").call()
    wedgedCommit
  }

  private def tree(commit: Commit) = { //TODO: make val?
    val currentCommitId = gitRepo.resolve(commit.value)

    val revWalk = new RevWalk(gitRepo)
    val revCommit = revWalk.parseCommit(currentCommitId)
    revCommit.getTree()
  }

  private def createTreeWalk(commit: Commit, path: String) = {
    val treeWalk = new TreeWalk(gitRepo)
    treeWalk.addTree(tree(commit))
    treeWalk.setRecursive(true)
    treeWalk.setFilter(PathFilter.create(path))
    treeWalk
  }

  /** Uses String.matches to match for variants */
  def search(regex: String): Set[Variant] = {
    val treeWalk = new TreeWalk(gitRepo)
    treeWalk.addTree(tree(commit))
    treeWalk.setRecursive(false)
    treeWalk.setFilter(PathFilter.create(MetadataDirName)) //TODO: check this!

    //TODO: recurse instead?
    var variants = Set.empty[Variant]

    while (treeWalk.next()) {
      if (treeWalk.isSubtree()) {
        treeWalk.enterSubtree()
      } else {
        val path = treeWalk.getPathString()
        if (path.startsWith(MetadataDirName) && path.endsWith("." + JsonFileEnding) && path.matches(regex)) {
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
    createTreeWalk(commit, getVariantsBlobPath(id, hash)).next()
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
    val treeWalk = createTreeWalk(commit, path)

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

    val treeWalk = createTreeWalk(commit, getVariantsBasePath(id))
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
    ArtifactDescriptorDirName + GitPathSep + level1Dir + GitPathSep + level2Dir + GitPathSep + hash.value + "." + JsonFileEnding
  }

  override def hasArtifactDescriptor(hash: Hash): Boolean = {
    createTreeWalk(commit, getArtifactDescriptorPath(hash)).next()
  }

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