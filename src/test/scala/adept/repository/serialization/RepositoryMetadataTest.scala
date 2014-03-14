package adept.repository.serialization

import org.scalatest.FunSuite
import org.scalatest.matchers.MustMatchers
import adept.resolution.models.Id
import adept.utils.Hasher
import adept.repository.GitRepository
import adept.repository.models._
import adept.resolution.models._
import adept.artifact.models._

class RepositoryMetadataTest extends FunSuite with MustMatchers {
  import adept.test.FileUtils.usingTmpDir

  test("Create and read repository metadata") {
    usingTmpDir { rootDir =>
      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
      repository.init()
      val id = Id("test/foo")

      val repositoryMetadata = ResolutionResultsMetadata(Seq(ResolutionResult(
          id = id,
          repository = RepositoryName("test"),
          commit = Commit("131321321"),
          variant = VariantHash(Hasher.hash("foo".getBytes))
      )))
      
      val hash = VariantHash(Hasher.hash("blah".getBytes))
      repository.add(repositoryMetadata.write(id, hash, repository))
      repository.commit("Added repository")
      import org.scalatest.OptionValues._
      ResolutionResultsMetadata.read(id, hash, repository, repository.getHead).value must be === repositoryMetadata
    }
  }
}