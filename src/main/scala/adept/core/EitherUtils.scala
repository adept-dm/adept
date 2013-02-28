package adept.core

object EitherUtils {
  def reduce[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
    s.foldLeft(Right(Nil): Either[A, List[B]]) {
      (acc, e) => for (xs <- acc.right; x <- e.right) yield x :: xs
    }.right.map(_.reverse)
}