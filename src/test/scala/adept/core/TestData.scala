package adept.core

import adept.core.models._

object TestData {
  val modules = Map(
      Module(Coordinates("test", "upper","1.0"), Metadata(Map("test" -> "yes")), Hash("unique1"), Hash("artifactHashUpper"), Set(Artifact("http://test.com/upper/location1.jar"))) -> 
        Set(Module(Coordinates("test", "dep2","2.42"), Metadata(Map("test" -> "foo", "some" -> "other")), Hash("unique2"),  Hash("artifactHash2"), Set(Artifact("http://test.com/foo/location1.jar"))),
            Module(Coordinates("test", "dep1","0.69"), Metadata(Map("random" -> "one")), Hash("unique3"), Hash("artifactHash1"), Set(Artifact("http://test.com/random/location1.jar"), Artifact("http://test.com/random/location2"))))
  )
  val repoName = "test"
  val parent: Module = modules.head._1
  
}
