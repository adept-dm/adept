package adept.core.resolution

import adept.core.models._
import adept.core._
import adept.test.TestDSL._
import adept.test.TestHelpers._
import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.ext.DefinedVariants

class CombinationsTest extends FunSuite with MustMatchers {
  test("combinations should work") {
    val testData = useTestData(
      V("A")("version" -> Set("1.0"))(),
      V("A")("version" -> Set("2.0"))(),
      V("B")("version" -> Set("1.0"))(),
      V("B")("version" -> Set("2.0"))(),
      V("C")("version" -> Set("1.0"))(),
      V("C")("version" -> Set("2.0"))(),
      V("D")("version" -> Set("1.0"))(),
      V("D")("version" -> Set("2.0"))(),
      V("E")("version" -> Set("1.0"))())
    val (dependencies, all) = testData
    val resolver = new Resolver(new DefinedVariants(all))
    
    println(resolver.combinations(Set(new Id("D"), new Id("E")), Set.empty, Map.empty).map(_.toList).toList)
    //D 1.0, E 1.0, D 2.0, D 1.0 & E 1.0, D 2.0 & E 1.0 
    println(resolver.combinations(Set(new Id("D"), new Id("E")), Set.empty, Map.empty).map(_.toList).toList.mkString("\n"))
    true
  }

}