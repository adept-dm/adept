package adept.repository

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.repository.models.RepositoryName
import adept.repository.models.Commit

class GitHelpersTest extends FunSuite with Matchers {
  import adept.test.FileUtils.usingTmpDir

  test("GitHelpers can find the latest commit") {
    usingTmpDir { tmpDir =>
      val repository = new GitRepository(tmpDir, RepositoryName("test-repo"))
      repository.init()
      val commit1 = repository.commit("Test 1")
      val commit2 = repository.commit("Test 2")
      val commit3 = repository.commit("Test 3")
      val commits = Set(commit2, commit3, commit1, Commit("something else"), Commit("something"))
      GitHelpers.lastestCommits(repository, commits) shouldEqual Set(commit3, Commit("something else"), Commit("something"))
    }
  }
}