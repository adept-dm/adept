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
        requirements = Seq(Requirement(Id("i/require/this"), constraints = Set(Constraint("binary-version", Set("2.1", "2.0"))))),
        exclusions = Seq(Id("foo"), Id("abc")))

      repository.add(variantMetadata.write(id, repository))
      repository.commit("Added variant")
      import org.scalatest.OptionValues._
      VariantMetadata.read(id, variantMetadata.hash, repository, repository.getHead).value shouldEqual variantMetadata.toVariant(id)
    }
  }

  test("Hash extraction in Git") {
    {
      val path = "variants/scala-library/config/javadoc/606d/da39/a3c9bf70b7ac19f407de89ff12edcd06996c390b4cd03c2b2a55aabc/variant.json"
      (path match {
        case VariantMetadata.HashExtractionRegex(id, level1, level2, level3) =>
          Some(level1 + level2 + level3)
        case _ => None
      }) shouldEqual Some("606dda39a3c9bf70b7ac19f407de89ff12edcd06996c390b4cd03c2b2a55aabc")
    }

    {

      val path = "variants/scala-library/606d/da39/a3c9bf70b7ac19f407de89ff12edcd06996c390b4cd03c2b2a55aabc/variant.json"
      (path match {
        case VariantMetadata.HashExtractionRegex(id, level1, level2, level3) =>
          Some(level1 + level2 + level3)
        case _ => None
      }) shouldEqual Some("606dda39a3c9bf70b7ac19f407de89ff12edcd06996c390b4cd03c2b2a55aabc")
    }
  }
}