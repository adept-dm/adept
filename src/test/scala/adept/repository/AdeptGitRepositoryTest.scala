package adept.repository

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.repository.models.VariantMetadata
import adept.repository.models.VariantMetadataTestData
import adept.repository.models.MetadataContent
import java.io.File
import adept.models._

class AdeptGitRepositoryTest extends FunSuite with MustMatchers {
  import adept.test.FileUtils.usingTmpDir

  test("Basic initialization test (checks basic file locking behavior)") {
    usingTmpDir { tmpDir =>
      val adeptGitRepo1 = new AdeptGitRepository(tmpDir, "foo")
      val adeptGitRepo2 = new AdeptGitRepository(tmpDir, "foo")

      adeptGitRepo1.isClean must be === true
      adeptGitRepo2.isClean must be === true

      adeptGitRepo1.getMostRecentCommit.commit must be === adeptGitRepo2.getMostRecentCommit.commit
    }
  }

  test("Basic git repository checks: add, update and compare") {
    usingTmpDir { tmpDir =>
      import adept.repository.models.VariantMetadataTestData.metadata

      val adeptGitRepo = new AdeptGitRepository(tmpDir, "adept-git-repo-basic")

      val commit1 = adeptGitRepo.updateMetadata({ content =>
        content.variantMetadata must be('empty)
        Seq.empty
      }, { content =>
        content.variantMetadata must be('empty)
        Seq(metadata.write(adeptGitRepo))
      }, "adding some new stuff")

      val commit2 = adeptGitRepo.updateMetadata({ content =>
        content.variantMetadata must have size (1)
        content.variantMetadata.toSeq.map {
          _.file(adeptGitRepo)
        }
      }, { content =>
        Seq(metadata.copy(attributes = metadata.attributes + Attribute("extra", Set("stuff"))).write(adeptGitRepo))
      }, "updating with extra stuff")

      commit1.canCompare(commit2) must be === true

      commit1 < commit2 must be === true
      commit1 > commit2 must be === false
      commit1 == commit2 must be === false
      commit2 < commit1 must be === false
      commit2 == commit1 must be === false
      commit2 > commit1 must be === true
    }

  }

  test("Basic scanning tests") {
//    import OptionValues._
//    usingTmpDir { tmpDir =>
//
//      import adept.repository.models.ConfiguredVariantsMetadataTestData.metadata
//
//      val adeptGitRepo = new AdeptGitRepository(tmpDir, "adept-git-repo-basic")
//
//      val commit1 = adeptGitRepo.updateMetadata({ content =>
//        content.artifactsMetadata must be('empty)
//        content.variantsMetadata must be('empty)
//        Seq.empty
//      }, { content =>
//        content.artifactsMetadata must be('empty)
//        content.variantsMetadata must be('empty)
//        Seq(metadata.write(adeptGitRepo))
//      }, "adding some new stuff")
//
//      val commit2 = adeptGitRepo.updateMetadata({ content =>
//        content.variantsMetadata must have size (1)
//        content.artifactsMetadata must be('empty)
//        content.variantsMetadata.toSeq.map {
//          _.file(adeptGitRepo)
//        }
//      }, { content =>
//        Seq(metadata.copy(attributes = metadata.attributes + Attribute("extra", Set("stuff"))).write(adeptGitRepo))
//      }, "updating with extra stuff")
//
//      val commit3 = adeptGitRepo.updateMetadata({ content =>
//        content.variantsMetadata must have size (1)
//        content.artifactsMetadata must be('empty)
//        content.variantsMetadata.toSeq.map {
//          _.file(adeptGitRepo)
//        }
//      }, { content =>
//        Seq(metadata.copy(attributes = metadata.attributes + Attribute("even-more-extra", Set("stuff**2"))).write(adeptGitRepo))
//      }, "updating with even more cool stuff")
//
//      adeptGitRepo.scanFirst { content =>
//        content.variantsMetadata.exists(_.id == Id("foo/bar"))
//      }.value._1 must be === commit3
//
//      val scanResult = adeptGitRepo.scan { content =>
//        content.variantsMetadata.exists(_.id == Id("foo/bar"))
//      }
//      scanResult must have size (3)
//
//      scanResult(0)._1 must be === commit3
//      scanResult(1)._1 must be === commit2
//      scanResult(2)._1 must be === commit1
//    }
    pending //TODO: fix failing test
  }

}