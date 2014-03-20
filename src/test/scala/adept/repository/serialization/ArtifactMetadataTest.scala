package adept.repository.serialization

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.resolution.models.Id
import adept.utils.Hasher
import adept.repository.GitRepository
import adept.repository.models._
import adept.resolution.models._
import adept.artifact.models._

class ArtifactMetadataTest extends FunSuite with Matchers {
  import adept.test.FileUtils.usingTmpDir

  test("Create and read artifact metadata") {
    usingTmpDir { rootDir =>
      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
      repository.init()
      val id = Id("test/foo")

      val artifactMetadata = ArtifactMetadata(1234L, Set("funfun"))
      
      val hash = ArtifactHash(Hasher.hash("blah".getBytes))
      repository.add(artifactMetadata.write(hash, repository))
      repository.commit("Added repository")
      import org.scalatest.OptionValues._
      ArtifactMetadata.read(hash, repository, repository.getHead).value shouldEqual artifactMetadata
    }
  }
}