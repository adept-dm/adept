package adept.core.operations

import org.scalatest._
import adept.core.operations._
import adept.core.models._

class MergeTest extends FunSuite with MustMatchers {
  import adept.core.tests.TestData._

  test("merging basic artifacts") {
    val artifacts1 = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url.no/hash2.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar")))
    val artifacts2 = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url2.no/hash2.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url2.no/hash2-sources.jar")))

    val res = MergeOperations.mergeArtifacts(artifacts1, artifacts2)
    res must have size (2)

    res must contain(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url.no/hash2.jar", "http://url2.no/hash2.jar")))
    res must contain(Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar", "http://url2.no/hash2-sources.jar")))
  }

  test("merging artifacts with different artifact types") {
    val artifacts1 = Set(Artifact(Hash("artihash2"), "zip", Set("compile"), Set("http://url.no/hash2.zip")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar")))
    val artifacts2 = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url2.no/hash2.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url2.no/hash2-sources.jar")))

    val res = MergeOperations.mergeArtifacts(artifacts1, artifacts2)

    res must have size (3)
    res must contain(Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar", "http://url2.no/hash2-sources.jar")))
  }

  test("merging artifacts where there are uniques") {
    val artifacts1 = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url.no/hash2.jar")), Artifact(Hash("artihash3"), "jar", Set("special"), Set("http://url.no/hash3.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url.no/hash2-sources.jar")))
    val artifacts2 = Set(Artifact(Hash("artihash2"), "jar", Set("compile"), Set("http://url2.no/hash2.jar")), Artifact(Hash("artihash4"), "jar", Set("special"), Set("http://url2.no/hash4.jar")), Artifact(Hash("artihash-sources"), "jar", Set("sources"), Set("http://url2.no/hash2-sources.jar")))

    MergeOperations.mergeArtifacts(artifacts1, artifacts2) must have size (4)
  }
  
}