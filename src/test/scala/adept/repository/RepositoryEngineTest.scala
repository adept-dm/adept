package adept.repository

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import java.io.File
import adept.core.models._
import adept.test.TestHelpers

class RepositoryEngineTest extends FunSuite with MustMatchers {
  import TestHelpers._

  test("basic variant read/write repo manager test") {
    import EitherValues._
    usingTempDir { tmpDir =>
      val repo = Repository(tmpDir, "test-repo")
      val variantA1 = Variant(Id("A"), Set(ArtifactRef(Hash("123"), Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("A1"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X"))))))
      val variantA2 = Variant(Id("A"), Set(ArtifactRef(Hash("123"), Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("A2"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X"))))))
      val variantB = Variant(Id("B"), Set(ArtifactRef(Hash("456"), Set(Attribute("master", Set("compile"))), Some("filenameB"))), Set(Attribute("version", Set("B", "Bogus"))), Set(Dependency(new Id("C"), Set(Constraint("version", Set("X"))))))

      repo.writeVariant(variantA1)
      repo.writeVariant(variantA2)
      repo.writeVariant(variantB)

      val manager = new SlowRepositoryEngine(tmpDir, Set(repo))

      manager.get(Id("A"), Set()) must have size (2)
      manager.get(Id("A"), Set(Constraint("version", Set("A2")))) must have size (1)
      manager.get(Id("A"), Set(Constraint("version", Set("X")))) must be('empty)

    }
  }

  test("basic artifact read/write repo manager test") {
    import EitherValues._
    usingTempDir { tmpDir =>
      val repo = Repository(tmpDir, "test-repo")
      val hashB1 = Hash("123b1")
      val hashB2 = Hash("123b2")
      
      val variantB = Variant(Id("B"), Set(ArtifactRef(hashB1, Set(Attribute("configuration", Set("compile", "runtime"))), Some("filenamb1.jar")), ArtifactRef(hashB2, Set(Attribute("configuration", Set("javadoc"))), None)), Set(Attribute("version", Set("1.0"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X"))))))
      repo.writeVariant(variantB)
      val artifactB1 = Artifact(hashB1, 4124, Set("http://location1"))
      val artifactB2 = Artifact(hashB2, 5234, Set("http://location2"))
      repo.writeArtifactDescriptor(artifactB1)
      repo.writeArtifactDescriptor(artifactB2)
      
      val manager = new SlowRepositoryEngine(tmpDir, Set(repo))

      manager.getArtifacts(variantB, Set(Constraint("configuration", Set("compile", "runtime", "javadoc")))) must have size(2)
      manager.getArtifacts(variantB, Set(Constraint("configuration", Set("compile", "runtime")))) must have size(1)
      manager.getArtifacts(variantB, Set(Constraint("configuration", Set("compile")))) must have size(1)
      manager.getArtifacts(variantB, Set(Constraint("configuration", Set("runtime")))) must have size(1)
      manager.getArtifacts(variantB, Set(Constraint("configuration", Set("javadoc")))) must have size(1)
      manager.getArtifacts(variantB, Set(Constraint("configuration", Set("foo")))) must have size(0)
      manager.getArtifacts(variantB, Set(Constraint("configuration", Set()))) must have size(0)
    }
  }

  def timer(f: => Unit): Long = {
    val time = System.currentTimeMillis
    f
    System.currentTimeMillis - time
  }

  test("basic bench") {
    import EitherValues._
    usingTempDir { tmpDir =>
      val repo = Repository(tmpDir, "test-repo")

      for (i <- 1 to 1000) {
        val variant = Variant(Id("A"), Set(ArtifactRef(Hash("123" + i), Set(Attribute("master", Set("compile"))), None)), Set(Attribute("version", Set("A" + i))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X"))))))
        repo.writeVariant(variant)
      }

      val manager = new SlowRepositoryEngine(tmpDir, Set(repo))

      println(timer(manager.get(Id("A"), Set()) must have size (1000)))
      println(timer(manager.get(Id("A"), Set(Constraint("version", Set("A2")))) must have size (1)))
      println(timer(manager.get(Id("A"), Set(Constraint("version", Set("X")))) must be('empty)))
    }
  }

}