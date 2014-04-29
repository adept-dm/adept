package adept.test

import adept.repository.models.VariantHash
import adept.utils.Hasher

object HashUtils {
  import scala.language.implicitConversions //this is test code
  implicit def asHash(s: String): VariantHash = VariantHash(Hasher.hash(s.getBytes))
}