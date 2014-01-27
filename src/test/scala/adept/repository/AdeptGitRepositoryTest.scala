package adept.repository

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers

class AdeptGitRepositoryTest extends FunSuite with MustMatchers {
  import adept.test.FileUtils.usingTmpDir

  test("Basic initialization test (checks basic locking behavior)") {
    usingTmpDir { tmpDir =>
      val adeptGitRepo1 = new AdeptGitRepository(tmpDir, "foo")
      val adeptGitRepo2 = new AdeptGitRepository(tmpDir, "foo")
      
      adeptGitRepo1.isClean must be === true
      adeptGitRepo2.isClean must be === true 

      adeptGitRepo1.getMostRecentCommit.commit must be === adeptGitRepo2.getMostRecentCommit.commit
    }
  }
  
  
}