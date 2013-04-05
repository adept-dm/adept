package adept.core

import scala.util._

private[adept] object TryHelpers {
  def reduce[B](s: Seq[Try[B]]): Try[Seq[B]] =
    s.foldLeft(Success(Nil): Try[Seq[B]]) {
      (acc, e) => for (xs <- acc; x <- e) yield x +: xs
    }.map(_.reverse)

}