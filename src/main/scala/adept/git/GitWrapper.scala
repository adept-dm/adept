package adept.git

import adept.repository.Commit
import java.io.File
import java.io.InputStream
import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.{ Repository => GitRepository }
import org.eclipse.jgit.revwalk.RevWalk

object GitWrapper {
  def apply(dir: File) = new GitWrapper(dir)
}


//TODO: I think this is a good abstarction
class GitWrapper(dir: File) {
  private lazy val git = Git.open(dir)
  
  private def withGitRepo[A](func: GitRepository => A) = {
    val repo = git.getRepository()
    try {
      func(repo)
    } finally {
      repo.close()
    }
  }
  
  private def tree(commit: Commit, repo: GitRepository) = {
    val currentCommitId = repo.resolve(commit.value)

    val revWalk = new RevWalk(repo)
    val revCommit = revWalk.parseCommit(currentCommitId)
    revCommit.getTree()
  }

  def read[A](commit: Commit, paths: Set[String], recurse: Boolean = false)(func: (Commit, InputStream) => A) = {
    withGitRepo { repo =>
      val currentTree = tree(commit, repo)
      
    }
  }

}