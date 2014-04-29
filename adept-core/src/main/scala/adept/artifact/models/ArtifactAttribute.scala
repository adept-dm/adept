package adept.artifact.models

import adept.utils.OrderingHelpers

case class ArtifactAttribute(name: String, values: Set[String])

object ArtifactAttribute {
  implicit val ordering: Ordering[ArtifactAttribute] = new Ordering[ArtifactAttribute] {
    def compare(x: ArtifactAttribute, y: ArtifactAttribute): Int = {
      if (x.name < y.name)
        -1
      else if (x.name > y.name)
        1
      else {
        assert(x.name == y.name)
        OrderingHelpers.stringSetCompare(x.values, y.values)
      }
    }
  }
}