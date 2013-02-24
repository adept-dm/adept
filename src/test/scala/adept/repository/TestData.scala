package adept.repository

import adept.repository._

object TestData {
  val descriptors = Map(
      Descriptor(Coordinates("test", "upper","1.0"), Metadata(Map("test" -> "yes")), Hash("unique1")) -> 
        Seq(Descriptor(Coordinates("test", "dep2","2.42"), Metadata(Map("test" -> "foo", "some" -> "other")), Hash("unique2")),
            Descriptor(Coordinates("test", "dep1","0.69"), Metadata(Map("random" -> "one")), Hash("unique3")))
  )
  val repo = "test"
  val parent: Descriptor = descriptors.head._1
}