package adept.repository

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.repository.models.RepositoryName

class GitHelpersTest extends FunSuite with Matchers {
  import adept.test.FileUtils.usingTmpDir

  test("GitHelpers can find the latest commit") {
    usingTmpDir { tmpDir =>
      val repository = new GitRepository(tmpDir, RepositoryName("test-repo"))
      repository.init()
      val commit1 = repository.commit("Test 1")
      val commit2 = repository.commit("Test 2")
      val commit3 = repository.commit("Test 3")
      val commits = Set(commit2, commit3, commit1)
      GitHelpers.lastestCommit(repository, commits) shouldEqual Some(commit3)
    }
  }
}