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

object LocalGitRepository {

  val MasterBranchName = "master"

  private[adept] def compressModifications(lines: Map[String, String]) = {
    var compressedStrings = Set.empty[Set[String]]
    var checked = Set.empty[String]

    def checkThenAdd(checkedValue: String, addedValue: String) = {
      val (newStrings, oldStrings) = compressedStrings.partition(_.contains(checkedValue)) //TODO: I think this  partition is the slowest part of this algo, because it re iterates every found set AGAIN. consider using a lookup map
      compressedStrings = oldStrings + (newStrings.flatten ++ Set(addedValue))
      checked += addedValue
    }

    lines.foreach {
      case (left, right) =>
        if (!(checked(left) || checked(right))) { //first time we see either value
          var currentSet = Set(left, right)
          checked ++= currentSet
          var currentValue: Option[String] = Some(right)

          //TODO: we can recurse here instead, but I wonder if it really makes it easier to read 
          //add all right-hand side values:
          while (currentValue.isDefined && !checked(currentValue.get)) {
            currentValue match {
              case Some(v) =>
                currentValue = lines.get(v)
                checked += v
                currentSet += v
              case None =>
                assert(true,
                  "While optimizing modifications table matched a None when it should never happen (concurrent issue?). Compressed strings:\n" + compressedStrings.mkString("\n") + "\n\n all lines: " + lines.mkString("\n"))
            }
          }
          compressedStrings += currentSet
        } else if (checked(right)) {
          checkThenAdd(right, left)
        } else if (checked(left)) {
          checkThenAdd(left, right)
        }
    }

    compressedStrings
  }
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

  //TODO: UGLY! (move to object)
  private def readModications(commit: Commit) = {
    //TODO: constants
    readBlobFile(commit, "history/modifications") { isr =>
      val reader = new BufferedReader(isr)
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
    }

    //    val revWalk = new RevWalk(gitRepo)
    //    println(gitRepo)
    //    val revCommit = revWalk.lookupCommit(gitRepo.resolve("HEAD"))
    //    val tree = revCommit.getTree()
    //    if (tree != null) {
    //      val treeWalk = new TreeWalk(gitRepo)
    //      treeWalk.addTree(tree)
    //      treeWalk.setRecursive(false)
    //      treeWalk.setFilter(PathFilter.create("history/modifications"))
    //      println("__>>>")
    //      if (treeWalk.next() && !treeWalk.isSubtree()) {
    //        println("tree on " + treeWalk.getPathString())
    //        readBlob(treeWalk) { is =>
    //          if (is != null) {
    //            val reader = new BufferedReader(new InputStreamReader(is))
    //            try {
    //              var lines = Vector.empty[String]
    //              var l = reader.readLine()
    //              while (l != null) {
    //                lines = lines :+ l
    //                l = reader.readLine()
    //              }
    //              println("read: " + lines.mkString("\n"))
    //              lines
    //            } finally {
    //              reader.close()
    //            }
    //          } else {
    //            Vector.empty[String]
    //          }
    //        }.right.get //TODO: fix right.get
    //      } else {
    //        Seq.empty[String]
    //      }
    //    } else Vector.empty[String]
  }

  import LocalGitRepository._
  
  private val HistoryDirName = "history"
  private val ModificationsFileName = "modifications"

  /**
   * wedge (using cherry picks and branches) a variant in after a commit
   *
   *  TODO: this code is awful Fredrik - promise yourself to fix... please!
   */
  private[adept] def wedge(variants: Set[Variant], commit: Commit, commitMsg: String, startPoint: String = MasterBranchName): Set[LocalGitRepository] = {

    def deletePreviousVariants(commit: Commit) = {
      //delete previous variants TODO: refactor!
      val previousVariants = variants.flatMap { v =>
        val foundVariants = readVariants(commit, v.id).right.get //TODO: right.get should be fixed
        if (foundVariants.size > 1) throw new Exception("Wedge of more than one variant is currently not supported. Found: " + foundVariants.mkString(" ")) //TODO: support this
        foundVariants
      }

      previousVariants.foreach(deleteVariant)
    }

    try {
      git.checkout().setName(MasterBranchName).call()

      val revWalk = new RevWalk(gitRepo)

      //TODO: clean up resources
      revWalk.markStart(revWalk.lookupCommit(gitRepo.resolve(startPoint)))

      val it = revWalk.iterator
      var finished = false
      var revCommits = Seq.empty[RevCommit]

      while (it.hasNext && !finished) {
        val revCommit = it.next()

        if (revCommit.name == commit.value || (commit.value == InitTag && revCommit.getParentCount() == 0)) { //TODO: this is an aweful hack because I cannot figure out how to get the sha1 for a tag
          finished = true
        } else {
          revCommits = revCommit +: revCommits
        }
      }
      val wedgeCommitMsg = "Wegde modifications"
      if (finished) {
        val branchName = "wedge-insert-" + Hash.calculate(variants.toSeq)
        git.branchList().call().iterator().next().getName()
        git.branchCreate().setStartPoint(commit.value).setName(branchName).call()

        val newBranchInit = git.checkout().setName(branchName).call()
        deletePreviousVariants(Commit(branchName))

        variants.foreach(writeVariant)

        //TODO: please fix this ugly code
        var prevCommit = git.commit.setMessage(commitMsg).call()
        var lastCommitMsg = commitMsg

        var mergedCommitMsg: Option[String] = None
        val modificationsMappings = revCommits.map { revCommit =>

          val cherryPickResult = git.cherryPick().include(revCommit).call()
          lastCommitMsg = revCommit.getFullMessage()
          mergedCommitMsg = Some(revCommit.getFullMessage())
          val amend = if (revCommit.getFullMessage() != wedgeCommitMsg) {
            deletePreviousVariants(Commit(prevCommit.name))
            true
          } else false
          prevCommit = git.commit().setAmend(amend).setMessage(lastCommitMsg).call()

          prevCommit -> revCommit
        }

        val allLines = modificationsMappings.map { case (a, b) => a.name + "=" + b.name }
        val finalCommits: Set[RevCommit] = if (allLines.nonEmpty) {
          val modificationsDir = new File(repoDir, HistoryDirName)
          if (!(modificationsDir.isDirectory() || modificationsDir.mkdirs())) throw new Exception("cannot create: " + modificationsDir)
          val append = true
          val writer = new BufferedWriter(new FileWriter(new File(modificationsDir, ModificationsFileName), append))
          try {
            writer.write(allLines.mkString("", "\n", "\n"))
            writer.flush()
            git.add().addFilepattern(HistoryDirName + GitPathSep + ModificationsFileName).call()
            val msg = mergedCommitMsg.get //should be here if allLines.nonEmpty is true
            Set(git.commit.setMessage(wedgeCommitMsg).call())
          } finally {
            writer.close()
          }
        } else Set.empty[RevCommit]

        git.branchRename().setOldName(MasterBranchName).setNewName("wedge-backup-" + Hash.calculate(variants.toSeq)).call()
        git.branchRename().setNewName(MasterBranchName).setOldName(branchName).call()
        finalCommits.map(revCommit => new LocalGitRepository(baseDir, name, Commit(revCommit.name))) ++
          modificationsMappings.map { case (newCommit, oldCommit) => new LocalGitRepository(baseDir, name, Commit(newCommit.name)) }
      } else {
        Set(new LocalGitRepository(baseDir, name, getMostRecentCommit))
      }
    } finally {
      git.checkout().setName(MasterBranchName).call()
    }
  }

  private lazy val EqualModificationPattern = """^([0-9a-f]+)\=([0-9a-f]+)$""".r

  def compare(that: LocalGitRepository): Int = { //TODO: we cannot really use Ordered and compare here. we might be larger/smaller/equal OR we can be not comparable.

    val branchName = MasterBranchName

    if (that.name != this.name) throw new Exception("Cannot compare 2 different repositories")

    val allModLines = that.readModications(that.commit).right.get ++ this.readModications(this.commit).right.get
    val allMods = LocalGitRepository.compressModifications(allModLines.map {
      case EqualModificationPattern(a, b) => a -> b
    }.toMap)

    def equalCommits(commitValue1: String, commitValue2: String): Boolean = {
      commitValue1 == commitValue2 || allMods.exists { commits =>
        commits.contains(commitValue1) && commits.contains(commitValue2)
      }
    }

    def checkMatch(current: RevCommit): Option[Commit] = {
      if (equalCommits(this.commit.value, current.name)) {
        Some(this.commit)
      } else if (equalCommits(that.commit.value, current.name)) {
        Some(that.commit)
      } else None
    }

    val revWalk = new RevWalk(gitRepo)
    revWalk.markStart(revWalk.lookupCommit(gitRepo.resolve(Commit.Head.value)))
    revWalk.setRevFilter(RevFilter.NO_MERGES)
    val it = revWalk.iterator()

    //TODO: fold instead?
    var first: Option[Commit] = None
    var second: Option[Commit] = None

    //TODO: factor this code I almost cry when I read it (knowing I wrote it) still time is scarce :(
    while (it.hasNext && (first.isEmpty || second.isEmpty)) {
      val current = it.next()
      if (first.isEmpty) {
        first = checkMatch(current)
      } else if (first.nonEmpty && second.isEmpty) {
        second = checkMatch(current)
      }
    }
    if (first.nonEmpty && second.nonEmpty) {
      if (first.get == this.commit) 1
      else if (second.get == this.commit) -1
      else 0
    } else {
      throw new Exception("Cannot compare " + this + " with " + that + ". Found values: " + first + " " + second)
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
      println("reading path: " + treeWalk.getPathString())
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

  private val IdPathRegEx = """^(.*)/.*?$""".r

  private def extractIdPath(path: String): Option[String] = {
    path match {
      case IdPathRegEx(v) => Some(v)
      case _ => None
    }
  }

  def readVariants(commit: Commit, id: Id): Either[String, Set[Variant]] = {
    import org.json4s.native.Serialization.read
    import adept.serialization.Formats._
    val idPath = getVariantsBasePath(id)
    val treeWalk = createTreeWalk(commit, idPath)

    //imperative style makes this easier to read:
    var result: Either[String, Set[Variant]] = Right(Set.empty[Variant])
    while (treeWalk.next()) {
      if (extractIdPath(treeWalk.getPathString()).getOrElse("") == idPath) {
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
    }
    treeWalk.release()
    result
  }

  override def readVariants(id: Id): Either[String, Set[Variant]] = {
    readVariants(commit, id)
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