package adept.repository

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import adept.repository.models.ConfiguredVariantsMetadata
import adept.repository.models.ConfiguredVariantsMetadataTestData

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
  
  test("Basic adding test") {
    usingTmpDir { tmpDir =>
      
      val adeptGitRepo = new AdeptGitRepository(tmpDir, "foo")
      
      println(adeptGitRepo.addMetadata(Some(ConfiguredVariantsMetadataTestData.metadata), None, releaseNotes = "My first release - I rock!"))
      
    }
  }
  
  
}