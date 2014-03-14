package adept.repository.serialization

import org.scalatest.FunSuite
import org.scalatest.Matchers
import adept.resolution.models.Id
import adept.utils.Hasher
import adept.repository.GitRepository
import adept.repository.models._
import adept.resolution.models._
import adept.artifact.models._

class VariantMetadataTest extends FunSuite with Matchers {
  import adept.test.FileUtils.usingTmpDir

  test("Create and read variant metadata") {
    usingTmpDir { rootDir =>
      val repository = new GitRepository(rootDir, RepositoryName("test-repo1"))
      repository.init()
      val id = Id("test/foo")
      val variantMetadata = VariantMetadata(
          attributes = Seq(Attribute("version", Set("1.0.0"))),
          artifacts = Seq(
              ArtifactRef(ArtifactHash(Hasher.hash("foo".getBytes)), Set(ArtifactAttribute("some-stuff", Set("value"))), Some("test-file.jar")), 
              ArtifactRef(ArtifactHash(Hasher.hash("blah".getBytes)), Set(ArtifactAttribute("other-stuff", Set("value 2"))), Some("test-file2.jar"))), 
          requirements = Seq(Requirement(Id("i/require/this"), constraints = Set(Constraint("binary-version", Set("2.1", "2.0")))))
      )
      
      repository.add(variantMetadata.write(id, repository))
      repository.commit("Added variant")
      import org.scalatest.OptionValues._
      VariantMetadata.read(id, variantMetadata.hash, repository, repository.getHead).value shouldEqual variantMetadata.toVariant(id)
    }
  }
}