package adept.repository.serialization

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.resolution.models.Id
import adept.utils.Hasher
import adept.repository.GitRepository
import adept.repository.models._
import adept.resolution.models._
import adept.artifact.models._
import adept.repository.Repository

class RankingMetadataTest extends FunSuite with Matchers {
  import adept.test.FileUtils.usingTmpDir

  test("Create and read ranking metadata") {
    usingTmpDir { rootDir =>
      import org.scalatest.OptionValues._
      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
      repository.init()
      val id = Id("test/foo")
      val rankingMetadata = RankingMetadata(
        variants = Seq(VariantHash(Hasher.hash("test1".getBytes)), VariantHash(Hasher.hash("test2".getBytes)), VariantHash(Hasher.hash("test3".getBytes)), VariantHash(Hasher.hash("test4".getBytes))))

      val rankId = RankingMetadata.getXRankId(id, repository, 0, 1).headOption.value
      repository.add(rankingMetadata.write(id, rankId, repository))
      repository.commit("Added ranking")
      RankingMetadata.read(id, rankId, repository, repository.getHead).value shouldEqual rankingMetadata
    }
  }

  test("Getting rankids is stable") {
    usingTmpDir { tmpDir =>
      val repository1 = new GitRepository(tmpDir, RepositoryName("test-repo1"))
      repository1.init()
      val id = Id("test/foo")
      val rankIds1 = RankingMetadata.getXRankId(id, repository1, 0, 5)
      rankIds1 should have size (5)
      RankingMetadata.getXRankId(id, repository1, 2, 3) should have size (3)

      val repository2 = new GitRepository(tmpDir, RepositoryName("test-repo1"))
      val rankIds21 = RankingMetadata.getXRankId(id, repository2, 0, 2)
      val rankIds22 = RankingMetadata.getXRankId(id, repository2, 2, 2)
      val rankIds23 = RankingMetadata.getXRankId(id, repository2, 4, 1)

      (rankIds21 ++ rankIds22 ++ rankIds23) shouldEqual rankIds1
    }
  }

  test("Listing rankids works") {
    usingTmpDir { tmpDir =>
      val repository = new GitRepository(tmpDir, RepositoryName("test-repo1"))
      repository.init()
      val id = Id("test/foo")
      val rankIds1 = RankingMetadata.getXRankId(id, repository, 0, 3)
      RankingMetadata.listRankIds(id, repository, repository.getHead) should have size(0)
      rankIds1.foreach { rankId =>
        repository.add(RankingMetadata(
          variants = Seq(VariantHash(Hasher.hash("test1".getBytes))))
          .write(id, rankId, repository))
        repository.commit("Adding rankings " + rankId)
      }

      RankingMetadata.listRankIds(id, repository, repository.getHead) shouldEqual rankIds1 
    }
  }

}