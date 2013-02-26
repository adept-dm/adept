package adept.core

object TestData {
  val modules = Map(
      Module(Coordinates("test", "upper","1.0"), Metadata(Map("test" -> "yes")), Hash("unique1")) -> 
        Seq(Module(Coordinates("test", "dep2","2.42"), Metadata(Map("test" -> "foo", "some" -> "other")), Hash("unique2")),
            Module(Coordinates("test", "dep1","0.69"), Metadata(Map("random" -> "one")), Hash("unique3")))
  )
  val repo = "test"
  val parent: Module = modules.head._1
}
