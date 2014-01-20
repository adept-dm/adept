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
import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.FileOutputStream
import java.io.FileWriter
import scala.math.Ordered

object Commit {
  val Head = Commit(Constants.HEAD)
}

case class Commit(value: String) extends AnyVal {
  override def toString = value
}

class LocalGitRepository(val baseDir: File, val name: String, val commitRef: Commit) extends Repository with Ordered[LocalGitRepository] {
  def isClean: Boolean = {
    git.status().call().isClean()
  }

  def nonEmpty: Boolean = { //more than one commit
    val logIt = git.log()
      .call()
      .iterator()
    if (logIt.hasNext()) {
      logIt.next()
      logIt.hasNext()
    } else false
  }

  private def getMostRecentCommit = {
    val logIt = git.log()
      .call()
      .iterator()
    Commit(logIt.next.getId().name)
  }

  val commit: Commit = { //TODO: change name of this (commit) and commitRef....
    try {
      if (commitRef == Commit(Constants.HEAD)) { //get actual ref of HEAD
        getMostRecentCommit
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
  private[adept] def commit(msg: String): LocalGitRepository = {
    val revCommit = git.commit()
      .setMessage(msg)
      .call()
    new LocalGitRepository(baseDir, name, Commit(revCommit.name))
  }

  //TODO: fix LocalGitRepository! this method doesn't make sense to have here, because now localgitrepo is bound to one commit. think through how it should work and fix!
  private[adept] def scan(id: Id)(predicate: Variant => Boolean) = {
    val currentGitRepo = gitRepo
    val revWalk = new RevWalk(currentGitRepo)
    revWalk.markStart(revWalk.lookupCommit(gitRepo.resolve(Constants.HEAD)))

    //skip merges
    revWalk.setRevFilter(RevFilter.NO_MERGES)
    val it = revWalk.iterator()
    var matchingCommit: Option[Commit] = None

    while (it.hasNext && matchingCommit.isEmpty) {
      val revCommit = it.next()
      val tree = revCommit.getTree()
      val treeWalk = new TreeWalk(currentGitRepo)
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
      treeWalk.release()
    }
    revWalk.release()
    currentGitRepo.close()
    matchingCommit.map(commit => new LocalGitRepository(baseDir = baseDir, name = name, commit))
  }

  //TODO: UGLY!
  private def readModications(branchName: String) = {
    val revWalk = new RevWalk(gitRepo)
    val revCommit = revWalk.lookupCommit(gitRepo.resolve(branchName)) //Head of new branch
    val tree = revCommit.getTree()
    if (tree != null) {

      val treeWalk = new TreeWalk(gitRepo)
      treeWalk.addTree(tree)
      treeWalk.setRecursive(false)
      treeWalk.setFilter(PathFilter.create("history/modifications"))

      if (treeWalk.next() && !treeWalk.isSubtree()) {
        readBlob(treeWalk) { is =>
          if (is != null) {
            val reader = new BufferedReader(new InputStreamReader(is))
            try {
              var lines = Vector.empty[String]
              var l = reader.readLine()
              while (l != null) {
                lines = lines :+ l
                l = reader.readLine()
              }
              lines
            } finally {
              reader.close()
            }
          } else {
            Vector.empty[String]
          }
        }.right.get //TODO: fix right.get
      } else {
        Seq.empty[String]
      }
    } else Vector.empty[String]
  }

  /**
   * wedge (using cherry picks and branches) a variant in after a commit
   *
   *  TODO: this code is awful Fredrik - promise yourself to fix... please!
   */
  private[adept] def wedge(variant: Variant, commit: Commit, startPoint: String = "master") = { //TODO: constants
    try {

      git.checkout().setName("master").call()

      val revWalk = new RevWalk(gitRepo)

      //TODO: clean up resources
      revWalk.markStart(revWalk.lookupCommit(gitRepo.resolve(startPoint)))

      val it = revWalk.iterator
      var finished = false
      var revCommits = Seq.empty[RevCommit]

      while (it.hasNext && !finished) {
        val revCommit = it.next()
        println("-> " + revCommit.name)

        if (revCommit.name == commit.value || (commit.value == InitTag && revCommit.getParentCount() == 0)) { //TODO: this is an aweful hack because I cannot figure out how to get the sha1 for a tag
          finished = true
        } else {
          revCommits = revCommit +: revCommits
        }
      }
      if (finished) {
        val branchName = "wedge-insert-" + Hash.calculate(variant)
        git.branchList().call().iterator().next().getName()
        git.branchCreate().setStartPoint(commit.value).setName(branchName).call()

        git.checkout().setName(branchName).call()

        writeVariant(variant)
        git.commit.setMessage("Wedged " + variant).call()

        val mapping = revCommits.map { revCommit =>

          val cherryPickResult = git.cherryPick().include(revCommit).call()

          revCommit -> cherryPickResult.getNewHead()
        }

        val allLines = readModications(branchName) ++ mapping.map { case (a, b) => a.name + "=" + b.name }

        if (allLines.nonEmpty) {
          println(allLines.mkString("\n"))
          val modificationsDir = new File(repoDir, "history")
          if (!(modificationsDir.isDirectory() || modificationsDir.mkdirs())) throw new Exception("cannot create: " + modificationsDir)
          val a = new BufferedWriter(new FileWriter(new File(repoDir, "history/modifications")))
          a.write(allLines.mkString("\n"))
          a.flush()
          git.add().addFilepattern("history/modifications").call()

          git.commit.setAmend(true).setMessage(">>>").call()
        }

        git.branchRename().setOldName("master").setNewName("wedge-backup-" + Hash.calculate(variant)).call()
        git.branchRename().setNewName("master").setOldName(branchName).call()

        println(finished + "   >>> " + revCommits)
      } else {
        Set.empty
      }
    } finally {
      git.checkout().setName("master").call()
    }
  }

  private lazy val EqualModificationPattern = """^(\d+)\=(\d+)$""".r

  def compare(that: LocalGitRepository): Int = { //TODO: we cannot really use Orderd and compare here. we might be larger/smaller/equal OR we can be not comparable.
    val branchName = "master"

    if (that.name != this.name) throw new Exception("Cannot compare 2 different repositories")

    //    val allModLines = that.readModications(branchName) ++ this.readModications(branchName)
    //    val allMods = allModLines.map {
    //      case EqualModificationPattern(a, b) => a -> b
    //    }.toMap

    val revWalk = new RevWalk(gitRepo)
    revWalk.markStart(revWalk.lookupCommit(gitRepo.resolve(Commit.Head.value)))
    revWalk.setRevFilter(RevFilter.NO_MERGES)
    val it = revWalk.iterator()
    var first: Option[Commit] = None 
    var second: Option[Commit] = None 
    
    while (it.hasNext && (first.isEmpty || second.isEmpty)) {
      val current = it.next()
      if (first.isEmpty) { //TODO: factor this code:
        if (current.name == this.commit.value) first = Some(this.commit)
        else if (current.name == that.commit.value) first = Some(that.commit) 
      } else if (first.nonEmpty && second.isEmpty) {
        if (current.name == this.commit.value) first = Some(this.commit)
        else if (current.name == that.commit.value) first = Some(that.commit) 
      }
    }
    if (first.nonEmpty && second.nonEmpty) {
      if (first.get == this.commit) 1
      else if (second.get == this.commit) -1
      else 0
    } else {
      throw new Exception("Cannot compare " + this + " with " + that)
    }
  }

  private def tree(commit: Commit) = { //TODO: make val?
    val currentCommitId = gitRepo.resolve(commit.value)

    //TODO: clean up resources!
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
    treeWalk.release()
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