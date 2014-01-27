package adept.models

object Ordering {

  private def stringSetCompare(x: Set[String], y: Set[String]) = {
    if (x.size == y.size) {
      x.toSeq.sorted.zip(y.toSeq.sorted).foldLeft(0) {
        case (res, (a, b)) =>
          if (res == 0) scala.math.Ordering.String.compare(a, b)
          else res
      }
    } else
      x.size - y.size
  }

  implicit val attributeOrdering: Ordering[Attribute] = new Ordering[Attribute] {
    def compare(x: Attribute, y: Attribute): Int = {
      if (x.name < y.name)
        -1
      else if (x.name > y.name)
        1
      else {
        assert(x.name == y.name)
        stringSetCompare(x.values, y.values)
      }
    }
  }

  implicit val constraintOrdering: Ordering[Constraint] = new Ordering[Constraint] {
    def compare(x: Constraint, y: Constraint): Int = {
      if (x.name < y.name)
        -1
      else if (x.name > y.name)
        1
      else {
        assert(x.name == y.name)
        stringSetCompare(x.values, y.values)
      }
    }
  }

  implicit val requirementOrdering: Ordering[Requirement] = new Ordering[Requirement] {
    def compare(x: Requirement, y: Requirement): Int = {
      if (x.id.value < y.id.value)
        -1
      else if (x.id.value > y.id.value)
        1
      else {
        assert(x.id.value == y.id.value)
        if (x.constraints.size == y.constraints.size) {
          x.constraints.toSeq.sorted.zip(y.constraints.toSeq.sorted).foldLeft(0) {
            case (res, (cx, cy)) =>
              if (res == 0) constraintOrdering.compare(cx, cy)
              else res
          }
        } else x.constraints.size - y.constraints.size
      }
    }
  }

  implicit val artifactRefOrdering: Ordering[ArtifactRef] = new Ordering[ArtifactRef] {
    def compare(x: ArtifactRef, y: ArtifactRef): Int = {
      if (x.hash.value < y.hash.value)
        -1
      else if (x.hash.value > y.hash.value)
        1
      else {
        assert(x.hash.value == y.hash.value)
        if (x.attributes.size == y.attributes.size) {
          val res = x.attributes.toSeq.sorted.zip(y.attributes.toSeq.sorted).foldLeft(0) {
            case (res, (a, b)) =>
              if (res == 0) attributeOrdering.compare(a, b)
              else res
          }
          if (res == 0) {
            val s = x.filename.size - y.filename.size
            if (s == 0) {
              scala.math.Ordering.String.compare(x.filename.toString, y.filename.toString)
            } else s
          } else res
        } else x.attributes.size - y.attributes.size
      }
    }
  }
}