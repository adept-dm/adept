package adept.core.models

import org.scalatest._
import org.scalatest.matchers._

class VariantHashTest extends FunSuite with MustMatchers {
  test("basic hashing") {
    val variantA1 = Variant(Id("A"), Set(ArtifactRef(Hash("a"), Set(Attribute("configuration", Set("compile")), Attribute("foo", Set("bar"))), None)), Set(Attribute("foo", Set("B")), Attribute("version", Set("A"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("X", "fii")))), Dependency(new Id("C"), Set(Constraint("version", Set("X"))))))
    val variantA2 = Variant(Id("A"), Set(ArtifactRef(Hash("a"), Set(Attribute("foo", Set("bar")), Attribute("configuration", Set("compile"))), None)), Set(Attribute("version", Set("A")), Attribute("foo", Set("B"))), Set(Dependency(new Id("B"), Set(Constraint("version", Set("fii", "X")))), Dependency(new Id("C"), Set(Constraint("version", Set("X"))))))
      
    variantA1 must be === variantA2
    Hash.calculate(variantA1) must be === Hash.calculate(variantA2)
  }
}