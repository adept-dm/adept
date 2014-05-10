package adept.repository.metadata

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.resolution.models.Id
import adept.hash.Hasher
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

      val rankId = RankId("test")
      repository.add(rankingMetadata.write(id, rankId, repository))
      repository.commit("Added ranking")
      RankingMetadata.read(id, rankId, repository, repository.getHead).value shouldEqual rankingMetadata
    }
  }

  test("Listing rankids works") {
    usingTmpDir { tmpDir =>
      val repository = new GitRepository(tmpDir, RepositoryName("test-repo1"))
      repository.init()
      val id = Id("test/foo")
      val rankIds1 = Set(RankId("2.1"), RankId("2.1 Scala 2.10"), RankId("2.2"))
      RankingMetadata.listRankIds(id, repository, repository.getHead) should have size(0)
      rankIds1.foreach { rankId =>
        repository.add(RankingMetadata(
          variants = Seq(VariantHash(Hasher.hash(("test1"+rankId.value).getBytes))))
          .write(id, rankId, repository))
        repository.commit("Adding rankings " + rankId)
      }

      RankingMetadata.listRankIds(id, repository, repository.getHead) shouldEqual rankIds1 
    }
  }

}