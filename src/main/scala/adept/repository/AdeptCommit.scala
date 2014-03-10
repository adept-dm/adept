//package adept.repository
//
//import adept.repository.models.Commit
//import org.eclipse.jgit.revwalk.RevCommit
//import org.eclipse.jgit.revwalk.RevWalk
//import org.eclipse.jgit.revwalk.filter.RevFilter
//
//case class CommitCompareException(commit1: AdeptCommit, commit2: AdeptCommit) extends Exception("Cannot compare commits: " + commit1 + " and " + commit2 + ".")
//
///**
// * Represents a comparable commit for a given `AdeptGitRepository`.
// */
//case class AdeptCommit private[adept] (repo: AdeptGitRepository, val commit: Commit) extends Ordered[AdeptCommit] {
//  override def toString = repo + "@" + commit.value 
//  
//  def canCompare(that: AdeptCommit): Boolean = {
//    this.repo.dir.getAbsolutePath == that.repo.dir.getAbsolutePath && this.repo.workingBranchName == that.repo.workingBranchName
//  }
//
//  private def equalCommits(commitValue1: String, commitValue2: String): Boolean = {
//    commitValue1 == commitValue2
//  }
//
//  private def checkMatch(current: RevCommit, thisCommit: Commit, thatCommit: Commit): Option[Commit] = {
//    if (equalCommits(thisCommit.value, current.name)) {
//      Some(thisCommit)
//    } else if (equalCommits(thatCommit.value, current.name)) {
//      Some(thatCommit)
//    } else None
//  }
//
//  private def returnValue(firstCommit: Commit, secondCommit: Commit, current: AdeptCommit) = {
//    if (firstCommit == current.commit) 1
//    else if (secondCommit == current.commit) -1
//    else 0
//  }
//
//  private def traverseRepo(thisAdeptCommit: AdeptCommit, thatAdeptCommit: AdeptCommit) = {
//    thisAdeptCommit.repo.usingGitRepo { gitRepo => //git repos are comparable, so we can use either one.
//      val revWalk = new RevWalk(gitRepo)
//      try {
//        revWalk.markStart(revWalk.lookupCommit(gitRepo.resolve(thisAdeptCommit.repo.workingBranchName)))
//        revWalk.setRevFilter(RevFilter.NO_MERGES)
//        val it = revWalk.iterator()
//
//        var first: Option[Commit] = None
//        var second: Option[Commit] = None
//
//        while (it.hasNext && (first.isEmpty || second.isEmpty)) {
//          val current = it.next()
//          if (first.isEmpty) {
//            first = checkMatch(current, thisAdeptCommit.commit, thatAdeptCommit.commit)
//            if (first.nonEmpty && thisAdeptCommit.commit == thatAdeptCommit.commit) {
//              second = first
//            }
//          } else if (first.nonEmpty && second.isEmpty) {
//            second = checkMatch(current, thisAdeptCommit.commit, thatAdeptCommit.commit)
//          }
//        }
//        first -> second
//
//      } finally {
//        revWalk.release()
//      }
//    }
//  }
//
//  def compare(that: AdeptCommit): Int = {
//    if (!canCompare(that)) throw CommitCompareException(this, that)
//    else {
//      val (thisFirst, thisSecond) = traverseRepo(this, that)
//      if (thisFirst.nonEmpty && thisSecond.nonEmpty) {
//        returnValue(thisFirst.get, thisSecond.get, this)
//      } else {
//        //this.repo branch did not have both commits, 
//        //perhaps the that.repo branch has them: 
//        val (thatFirst, thatSecond) = traverseRepo(that, this)
//        if (thatFirst.nonEmpty && thatSecond.nonEmpty) {
//          returnValue(thatFirst.get, thatSecond.get, that)
//        } else {
//          throw new CommitCompareException(this, that)
//        }
//      }
//    }
//  }
//}