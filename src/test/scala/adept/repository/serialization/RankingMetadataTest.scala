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

}