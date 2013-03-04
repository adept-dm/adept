package adept.core

object TestData {
  val modules = Map(
      Module(Coordinates("test", "upper","1.0"), Metadata(Map("test" -> "yes")), Hash("artifactHashUpper"), Set(Artifact("http://test.com/upper/location1.jar")), Hash("unique1")) -> 
        Set(Module(Coordinates("test", "dep2","2.42"), Metadata(Map("test" -> "foo", "some" -> "other")),  Hash("artifactHash2"), Set(Artifact("http://test.com/foo/location1.jar")), Hash("unique2")),
            Module(Coordinates("test", "dep1","0.69"), Metadata(Map("random" -> "one")),  Hash("artifactHash1"), Set(Artifact("http://test.com/random/location1.jar"), Artifact("http://test.com/random/location2")),  Hash("unique3")))
  )
  val repoName = "test"
  val parent: Module = modules.head._1
}
