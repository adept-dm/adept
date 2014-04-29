package adept.utils

private[adept] object OrderingHelpers {
  def stringSetCompare(x: Set[String], y: Set[String]) = {
    if (x.size == y.size) {
      x.toSeq.sorted.zip(y.toSeq.sorted).foldLeft(0) {
        case (res, (a, b)) =>
          if (res == 0) scala.math.Ordering.String.compare(a, b)
          else res
      }
    } else
      x.size - y.size
  }
}