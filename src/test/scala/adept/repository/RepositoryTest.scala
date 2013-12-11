package adept.repository

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import java.io.File
import adept.core.models._
import adept.test.TestHelpers

class RepositoryTest extends FunSuite with MustMatchers {
  import TestHelpers._

  test("basic read/write repo test") {
    import EitherValues._
    usingTempDir { tmpDir =>
      val repo = Repository(tmpDir, "test-repo")
      val hashA = Hash("123")
      val variantA = Variant(Id("A"), Set(ArtifactRef(hashA, Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("A"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X"))))))
      val variantB = Variant(Id("B"), Set(ArtifactRef(Hash("456"), Set(Attribute("master", Set("compile"))), Some("filenameB"))), Set(Attribute("version", Set("B", "Bogus"))), Set(Dependency(new Id("C"), Set(Constraint("version", Set("X"))))))

      repo.writeVariant(variantA)
      repo.writeVariant(variantB)

      repo.readVariant(variantA.id, Hash.calculate(variantA)).right.value must be === variantA
      repo.readVariant(variantB.id, Hash.calculate(variantB)).right.value must be === variantB 
      
      val artifactA = Artifact(hashA, 123456, Set("http://foo"))
      repo.writeArtifactDescriptor(artifactA)
      repo.readArtifactDescriptor(hashA).right.value must be === artifactA
    }
  }

}