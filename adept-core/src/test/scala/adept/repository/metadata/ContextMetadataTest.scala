package adept.repository.metadata

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.resolution.models.Id
import adept.hash.Hasher
import adept.repository.GitRepository
import adept.repository.models._
import adept.resolution.models._
import adept.artifact.models._

class ContextMetadataTest extends FunSuite with Matchers {
  import adept.test.FileUtils.usingTmpDir

  test("Create and read repository metadata") {
    usingTmpDir { rootDir =>
      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
      repository.init()
      val id = Id("test/foo")

      val repositoryMetadata = ContextMetadata(Seq(ContextValue(
          id = id,
          repository = RepositoryName("test"),
          commit = Some(Commit("131321321")),
          variant = VariantHash(Hasher.hash("foo".getBytes))
      )))
      
      val hash = VariantHash(Hasher.hash("blah".getBytes))
      repository.add(repositoryMetadata.write(id, hash, repository))
      repository.commit("Added repository")
      import org.scalatest.OptionValues._
      ContextMetadata.read(id, hash, repository, repository.getHead).value shouldEqual repositoryMetadata
    }
  }
}
