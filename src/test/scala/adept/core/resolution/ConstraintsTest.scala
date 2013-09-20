package adept.core.resolution

import adept.core.models._
import adept.core._
import adept.test.TestDSL._
import adept.test.TestHelpers._
import adept.test.DefinedVariants
import org.scalatest._
import org.scalatest.matchers.MustMatchers

class ConstraintsTest extends FunSuite with MustMatchers {

  test("basic use case") {
    val resolver = load(useTestData(
      R("A")("version" -> "V")( //add a variant and a root dependency
        X("B")()), //add a dependency on B (any variant)

      V("B")("version" -> "X")()) //create a variant (not added as a dependency)
      )

    checkResolved(resolver, Set("A", "B"))
    checkUnresolved(resolver, Set())
  }

  test("basic transitivity") {
    val resolver = load(useTestData(
      R("A")("version" -> "V")(
        X("B")()),
      V("B")("version" -> "X")(
        X("C")("version" -> "X")),
      V("C")("version" -> "X")(
        X("D")()),
      V("C")("version" -> "Y")(
        X("D")()),
      V("D")("version" -> "X")()))

    checkResolved(resolver, Set("A", "B", "C", "D"))
    checkUnresolved(resolver, Set())
    checkVariants(resolver, "D" -> ("version" -> "X"))
  }

  test("basic under constrained") {
    val resolver = load(useTestData(
      R("A")("version" -> "V")(
        V("B")("binary-version" -> "X")()),

      V("B")("version" -> "1.0", "binary-version" -> "X")(),
      V("B")("version" -> "1.1", "binary-version" -> "X")()))

    checkResolved(resolver, Set("A"))
    checkUnresolved(resolver, Set("B"))
  }

  test("basic over constrained") {
    val resolver = load(useTestData(
      R("A")("version" -> "V")(
        V("B")("version" -> "X")()),

      R("B")("version" -> "Y")()))

    checkResolved(resolver, Set("A"))
    checkUnresolved(resolver, Set("B"))
  }

  test("transitive, multi variant") {
    val resolver = load(useTestData(
      R("A")("version" -> "V")(
        X("B")()),
      V("B")("version" -> "X")(
        X("C")("version" -> "X")),
      V("C")("version" -> "X")(
        X("D")()),
      V("C")("version" -> "Y")(
        X("D")()),
      V("D")("version" -> "X")()))

    checkResolved(resolver, Set("A", "B", "C", "D"))
    checkUnresolved(resolver, Set())
  }

  test("basic cyclic dependencies") {
    val resolver = load(useTestData(
      R("A")("version" -> "V")(
        X("B")()),

      R("B")("version" -> "X")(
        X("A")("version" -> "V"))))

    checkResolved(resolver, Set("A", "B"))
    checkUnresolved(resolver, Set())
  }

  test("cyclic dependencies, multiple variants possible") {
    val resolver = load(useTestData(
      R("A")("version" -> "V")(
        X("B")("version" -> "Y")),

      R("B")("version" -> "Y")(
        V("A")()()),

      V("A")("version" -> "X")(
        V("C")("version" -> "Z")())))

    checkResolved(resolver, Set("A", "B"))
    checkUnresolved(resolver, Set())
  }
}