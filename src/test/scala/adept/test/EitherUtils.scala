package adept.test

object EitherUtils {
  import scala.language.implicitConversions //it is OK we are in test configuration only
  implicit class EitherHandler[A](either: Either[_, A]) {
    def failOnLeft: A = { //this one is better than org.scalatest.EitherValues, because EitherValues does not output what left was
      assert(either.isRight, "Required Right, but got: " + either)
      either.right.get
    }
  }
}