package adept.repository

import org.scalatest.matchers.MustMatchers
import org.scalatest._
import adept.core.models._
import adept.core.resolution.Resolver
import net.sf.ehcache.CacheManager

class AdeptRepositoryManagerTest extends FunSuite with MustMatchers {
  import adept.test.TestHelpers._

  test("example 1") {
    REMOVEMEusingTempDir { tmpDir =>
      val cacheManager = CacheManager.create()
      try {

        val baseDir = tmpDir
        val repoName1 = "foo1"
        val repoName2 = "foo2"
        val repoHandle = AdeptRepositoryManager.init(baseDir, repoName1) ++ AdeptRepositoryManager.init(baseDir, repoName2)
        val engine = AdeptRepositoryManager.open(baseDir, repoHandle, cacheManager)
        val resolver = new Resolver(engine)
        val variantA = Variant(Id("Aurora/pretty"), Set(ArtifactRef(Hash("abc123"), Set(Attribute("configuration", Set("compile", "runtime"))), Some("filenama1.jar")), ArtifactRef(Hash("aartihash123"), Set(Attribute("configuration", Set("javadoc"))), None)), Set(Attribute("version", Set("1.0"))), Set(Dependency(new Id("Bobo/foo/zoo"), Set(Constraint("version", Set("2.0"))))))
        val variantB1 = Variant(Id("Bobo/foo/zoo"), Set(ArtifactRef(Hash("456def"), Set(Attribute("configuration", Set("compile", "runtime"))), Some("filenamb1.jar")), ArtifactRef(Hash("b1artihash456"), Set(Attribute("configuration", Set("javadoc"))), None)), Set(Attribute("version", Set("2.0"))), Set.empty)
        val variantB2 = Variant(Id("Bobo/foo/zoo"), Set(ArtifactRef(Hash("589ghi"), Set(Attribute("configuration", Set("compile", "runtime"))), Some("filenamb2.jar")), ArtifactRef(Hash("b2artihash789"), Set(Attribute("configuration", Set("javadoc"))), None)), Set(Attribute("version", Set("2.0"))), Set.empty)

        engine.addVariant(repoName1, variantA)

        //add some nested dependencies (simple config):
        engine.addVariant(repoName1, Variant(Id("Aurora/pretty/config/compile"), Set(ArtifactRef(Hash("abc123"), Set(Attribute("configuration", Set("compile", "runtime"))), Some("filenama1.jar")), ArtifactRef(Hash("aartihash123"), Set(Attribute("configuration", Set("javadoc"))), None)), Set(Attribute("version", Set("1.0"))), Set(Dependency(new Id("Bobo/foo/zoo"), Set(Constraint("version", Set("2.0")))))))
        unresolved(resolver.resolve(Set(Dependency(Id("Aurora/pretty"), Set.empty))))

        engine.addVariant(repoName1, variantB1)
        engine.commit("fixed required meta-data")
        resolved(resolver.resolve(Set(Dependency(Id("Aurora/pretty"), Set.empty))))

        engine.addVariant(repoName2, variantB2)
        engine.commit("an extra similar but the same variant in " + repoName2)
        unresolved(resolver.resolve(Set(Dependency(Id("Aurora/pretty"), Set.empty))))

      } finally {
        cacheManager.shutdown()
      }
    }

  }
}