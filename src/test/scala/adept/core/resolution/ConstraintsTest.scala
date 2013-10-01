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
        X("B")("binary-version" -> "X")),

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

  test("resolver.resolve basic consistency") {
    val (dependencies1, first) = useTestData(
      R("A")("version" -> "V")(
        X("B")("version" -> "Y")))

    val (dependencies2, second) = useTestData(
      R("B")("version" -> "Y")(
        V("A")()()),

      V("A")("version" -> "X")(
        V("C")("version" -> "Z")()))

    val resolver = new Resolver(new DefinedVariants(first ++ second))
    resolver.resolve(dependencies1)
    resolver.resolve(dependencies2)

    checkResolved(resolver, Set("A", "B"))
    checkUnresolved(resolver, Set())
  }

  test("nested constraints") {
    //B is unconstrained, but D forces C v 3.0, only B v 1.0 is constrained on C v 3.0 so B v 1.0 must be used:
    val resolver = load(useTestData(
      R("A")("v" -> "1.0")(
        X("B")(),
        X("C")(),
        X("D")(),
        X("E")()),
      V("B")("v" -> "1.0")(
        X("C")("v" -> "2.0")),
      V("B")("v" -> "2.0")(
        X("C")("v" -> "3.0")),
      V("E")("v" -> "1.0")(
        X("D")("v" -> "1.0"),
        X("B")()),
      V("D")("v" -> "2.0")(
        V("C")("v" -> "2.0")()),
      V("D")("v" -> "1.0")(
        V("C")("v" -> "3.0")())))

    checkUnresolved(resolver, Set())
    checkResolved(resolver, Set("A", "B", "C", "D", "E"))
  }

  test("multiple nested constraints") {
    //B is under-constrained initially and so is F, but since E requires D v 1.0
    //and D 1.0 requires C 3.0, only B 2.0 and F 2.0 can be used with C 3.0
    //this graph should be resolved
    val resolver = load(useTestData(
      R("A")("v" -> "1.0")(
        X("B")(),
        X("C")(),
        X("D")(),
        X("E")()),
      V("E")("v" -> "1.0")(
        X("D")("v" -> "1.0")), //requires D 1.0

      V("C")("v" -> "2.0")(),
      V("C")("v" -> "3.0")(),

      V("D")("v" -> "2.0")(
        X("C")("v" -> "2.0")),
      V("D")("v" -> "1.0")(
        X("C")("v" -> "3.0")), //requires C 3.0

      V("B")("v" -> "1.0")(
        X("C")("v" -> "2.0"),
        X("F")()),
      V("B")("v" -> "2.0")(
        X("C")("v" -> "3.0"), //requires C 3.0
        X("F")()),
      V("F")("v" -> "1.0")(
        X("C")("v" -> "2.0")),
      V("F")("v" -> "2.0")(
        X("C")("v" -> "3.0")))) //requires C 3.0

    checkUnresolved(resolver, Set())
    checkResolved(resolver, Set("A", "B", "C", "D", "E", "F"))
  }

  test("solving") {
    val resolver = load(useTestData(
      R("A")("v" -> "1.0")(
        X("B")(),
        X("C")("v" -> "2.0")),

      V("C")("v" -> "2.0")(),
      V("C")("v" -> "3.0")(),

      V("B")("v" -> "1.0")(
        X("C")("v" -> "2.0")),
      V("B")("v" -> "2.0")(
        X("C")("v" -> "3.0"))))

    println("UC: " + resolver.states.map(_.underconstrained))
    println("OC: " + resolver.states.map(_.overconstrained))
    println("R: " + resolver.states.map(_.resolved))
    println(resolver.states.map(_.globalConstraints.mkString("\n")).mkString("\n\n\n"))

    checkUnresolved(resolver, Set())
    checkResolved(resolver, Set("A", "B", "C", "D", "E", "F"))
  }

  test("solving2") {
    val resolver = load(useTestData(
      R("A")("v" -> "1.0")(
        X("B")(),
        X("C")(),
        X("D")(),
        X("E")("v" -> "2.0")),

      V("E")("v" -> "2.0")(),
      V("E")("v" -> "3.0")(),

      V("B")("v" -> "1.0")(
        X("E")("v" -> "2.0")),
      V("B")("v" -> "2.0")(
        X("E")("v" -> "3.0")),

      V("C")("v" -> "1.0")(
        X("E")("v" -> "2.0")),
      V("C")("v" -> "2.0")(
        X("E")("v" -> "3.0")),

     V("D")("v" -> "1.0")(
        X("E")("v" -> "2.0")),
      V("D")("v" -> "2.0")(
        X("E")("v" -> "3.0"))))
/*
    println("UC: " + resolver.states.map(_.underconstrained))
    println("OC: " + resolver.states.map(_.overconstrained))
    println("R: " + resolver.states.map(_.resolved))
    println(resolver.states.map(_.globalConstraints.mkString("\n")).mkString("\n\n\n"))
*/
    checkUnresolved(resolver, Set())
    checkResolved(resolver, Set("A", "B", "C", "D", "E", "F"))
  }

}