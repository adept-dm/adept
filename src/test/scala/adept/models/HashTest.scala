package adept.models

import org.scalatest._
import org.scalatest.matchers._

class HashTest extends FunSuite with MustMatchers {
  test("basic hashing") {
    val variantA1 = Variant(Id("A"), artifacts = Set(ArtifactRef(Hash("a"), Set(Attribute("configuration", Set("compile")), Attribute("foo", Set("bar"))), None)), attributes = Set(Attribute("foo", Set("B")), Attribute("version", Set("A"))), requirements = Set(Requirement(new Id("B"), Set(Constraint("version", Set("X", "fii")))), Requirement(new Id("C"), Set(Constraint("version", Set("X"))))))
    val variantA2 = Variant(Id("A"), artifacts = Set(ArtifactRef(Hash("a"), Set(Attribute("foo", Set("bar")), Attribute("configuration", Set("compile"))), None)), attributes = Set(Attribute("version", Set("A")), Attribute("foo", Set("B"))), requirements = Set(Requirement(new Id("B"), Set(Constraint("version", Set("fii", "X")))), Requirement(new Id("C"), Set(Constraint("version", Set("X"))))))

    variantA1 must be === variantA2
    Hash.calculate(variantA1) must be === Hash.calculate(variantA2)
  }
}