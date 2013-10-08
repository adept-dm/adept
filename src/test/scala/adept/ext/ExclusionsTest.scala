package adept.ext

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.test.TestDSL._
import adept.test.TestHelpers._
import adept.core.models._

class ExclusionsTest extends FunSuite with MustMatchers {

  test("basic exclusions") {
    val result = load(useTestData(
      R("A")("version" -> "V", "organization" -> "foo.com", "name" -> "A")(
        X("B")()),
      V("B")("version" -> "X", "organization" -> "org.loo", "name" -> "B")(
        X("C")("version" -> "X")),
      V("C")("version" -> "X", "organization" -> "boo.foo", "name" -> "C")(
        X("D")()),
      V("C")("version" -> "Y", "organization" -> "boo.foo", "name" -> "C")(
        X("D")()),
      V("D")("version" -> "X", "organization" -> "bar", "name" -> "D")()))

   val state = resolved(result)
   val variants = state.resolvedVariants ++ state.forcedVariants
   val (newVariants, includedVariants) = Extensions.exclude(state.graph, variants, query = Query(("organization" -> "boo.foo")))
   val newIds = newVariants.map( variant => variant.moduleId -> variant.dependencies.map(_.id)) 
   newIds must be === Set(
       "B" -> Set()
   )
   val includedIds = includedVariants.map{ case (_, variant) => variant.moduleId -> variant.dependencies.map(_.id)}.toSet[(String, Set[String])]
   includedIds must be === Set(
       "A" -> Set("B"),
       "B" -> Set()
   )
  }

}