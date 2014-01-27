package adept.resolution

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.models._
import adept.resolution.models._
import adept.repository._

class ResolverTest extends FunSuite with MustMatchers {
  import adept.test.ResolverUtils._

  val version = "version"

  test("Very simple resolution works correctly") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("V")),
        requirements = Set("B" -> Set.empty[Constraint])),

      Variant("B", Set(version -> Set("X")),
        requirements = Set("B" -> Set.empty[Constraint])))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(version, Set("V"))))

    val result = resolve(requirements, variants)
    checkResolved(result, Set("A", "B"))
    checkVariants(result, "A", version -> Set("V"))
    checkVariants(result, "B", version -> Set("X"))
  }

  test("Internal combinations method works as expected") {
    val d10 = Variant("D", Set(version -> Set("1.0")))
    val d20 = Variant("D", Set(version -> Set("2.0")))
    val e10 = Variant("E", Set(version -> Set("1.0")))

    val variants: Set[Variant] = Set(
      Variant("A", Set(version -> Set("1.0"))),
      Variant("A", Set(version -> Set("2.0"))),
      Variant("B", Set(version -> Set("1.0"))),
      Variant("B", Set(version -> Set("2.0"))),
      Variant("C", Set(version -> Set("1.0"))),
      Variant("C", Set(version -> Set("2.0"))),
      d10,
      d20,
      e10)

    val resolver = getResolver(variants)

    val combinations = resolver.combinations(Set(new Id("D"), new Id("E")), Set.empty, Map.empty).map(_.toSet).toList
    combinations(0) must be === Set(List(d10), List(d20), List(e10))

    //FIXME: this one should not fail if I am right?
    //D 1.0, E 1.0, D 2.0, D 1.0 & E 1.0, D 2.0 & E 1.0
    combinations(1) must be === Set(List(d10, e10), List(d20, e10))
  }

  test("All transitive variants are resolved correctly") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("V")),
        requirements = Set("B" -> Set.empty[Constraint])),

      Variant("B", Set(version -> Set("X")),
        requirements = Set("C" -> Set[Constraint](version -> Set("X")))),

      Variant("C", Set(version -> Set("X")),
        requirements = Set("D" -> Set.empty[Constraint])),
      Variant("C", Set(version -> Set("Y")),
        requirements = Set("D" -> Set.empty[Constraint])),

      Variant("D", Set(version -> Set("Z")),
        requirements = Set.empty))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(version, Set("V"))))

    val result = resolve(requirements, variants)
    checkResolved(result, Set("A", "B", "C", "D"))
    checkVariants(result, "D", version -> Set("Z"))
  }

  test("Simple under-constrained results behaves correctly") {
    val binaryVersion = "binary-version"

    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("1.0.0")),
        requirements = Set("B" -> Set[Constraint](binaryVersion -> Set("1.0")))),

      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0"))),
      Variant("B", Set(version -> Set("1.0.1"), binaryVersion -> Set("1.0"))))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(version, Set("1.0.0"))))

    val result = resolve(requirements, variants)
    checkResolved(result, Set("A"))
    checkUnresolved(result, Set("B"))
  }

  //  test("basic over constrained") {
  //    val result = load(useTestData(
  //      R("A")("version" -> Set("V"))(
  //        V("B")("version" -> Set("X"))()),
  //
  //      R("B")("version" -> Set("Y"))()))
  //
  //    println(result)
  //    val state = unresolved(result)
  //    checkResolved(state, Set("A"))
  //    checkUnresolved(state, Set("B"))
  //  }
  //
  //  test("transitive, multi variant") {
  //    val result = load(useTestData(
  //      R("A")("version" -> Set("V"))(
  //        X("B")()),
  //      V("B")("version" -> Set("X"))(
  //        X("C")("version" -> Set("X"))),
  //      V("C")("version" -> Set("X"))(
  //        X("D")()),
  //      V("C")("version" -> Set("Y"))(
  //        X("D")()),
  //      V("D")("version" -> Set("X"))()))
  //
  //    val state = resolved(result)
  //    checkResolved(state, Set("A", "B", "C", "D"))
  //    checkUnresolved(state, Set())
  //  }
  //
  //  test("basic cyclic dependencies") {
  //    val result = load(useTestData(
  //      R("A")("version" -> Set("V"))(
  //        X("B")()),
  //
  //      R("B")("version" -> Set("X"))(
  //        X("A")("version" -> Set("V")))))
  //
  //    println(result)
  //    val state = resolved(result)
  //
  //    checkResolved(state, Set("A", "B"))
  //    checkUnresolved(state, Set())
  //  }
  //
  //  test("cyclic dependencies, multiple variants possible") {
  //    val result = load(useTestData(
  //      R("A")("version" -> Set("V"))(
  //        X("B")("version" -> Set("Y"))),
  //
  //      R("B")("version" -> Set("Y"))(
  //        V("A")()()),
  //
  //      V("A")("version" -> Set("X"))(
  //        V("C")("version" -> Set("Z"))())))
  //
  //    println(result)
  //    val state = resolved(result)
  //    checkResolved(state, Set("A", "B"))
  //    checkUnresolved(state, Set())
  //  }
  //
  //  //  test("resolver.resolve basic consistency") {
  //  //    val (dependencies1, first) = useTestData(
  //  //      R("A")("version" -> Set("V"))(
  //  //        X("B")("version" -> Set("Y"))))
  //  //
  //  //    val (dependencies2, second) = useTestData(
  //  //      R("B")("version" -> Set("Y"))(
  //  //        V("A")()()),
  //  //
  //  //      V("A")("version" -> Set("X"))(
  //  //        V("C")("version" -> Set("Z"))()))
  //  //
  //  //    val resolver = new Resolver(new DefinedVariants(first ++ second))
  //  //    val result1 = resolver.resolve(dependencies1)
  //  //    val state1 = resolved(result1)
  //  //    val result2 = resolver.resolve(dependencies2, Some(state1))
  //  //
  //  //    val state2 = resolved(result2)
  //  //
  //  //    checkResolved(state2, Set("A", "B"))
  //  //    checkUnresolved(state2, Set())
  //  //  }
  //
  //  test("nested constraints") {
  //    //B is unconstrained, but D forces C v 3.0, only B v 1.0 is constrained on C v 3.0 so B v 1.0 must be used:
  //    val result = load(useTestData(
  //      R("A")("v" -> Set("1.0"))(
  //        X("B")(),
  //        X("C")(),
  //        X("D")(),
  //        X("E")()),
  //      V("B")("v" -> Set("1.0"))(
  //        X("C")("v" -> Set("2.0"))),
  //      V("B")("v" -> Set("2.0"))(
  //        X("C")("v" -> Set("3.0"))),
  //      V("E")("v" -> Set("1.0"))(
  //        X("D")("v" -> Set("1.0")),
  //        X("B")()),
  //      V("D")("v" -> Set("2.0"))(
  //        V("C")("v" -> Set("2.0"))()),
  //      V("D")("v" -> Set("1.0"))(
  //        V("C")("v" -> Set("3.0"))())))
  //
  //    val state = resolved(result)
  //
  //    println(state)
  //    checkResolved(state, Set("A", "B", "C", "D", "E"))
  //    checkUnresolved(state, Set())
  //  }
  //
  //  test("nested under-constrained path find") {
  //    //B is under-constrained initially and so is F, but since E requires D v 1.0
  //    //and D 1.0 requires C 3.0, only B 2.0 and F 2.0 can be used with C 3.0
  //    //this graph should be resolved
  //    val result = load(useTestData(
  //      R("A")("v" -> Set("1.0"))(
  //        X("B")(),
  //        X("C")(),
  //        X("D")(),
  //        X("E")()),
  //      V("E")("v" -> Set("1.0"))(
  //        X("D")("v" -> Set("1.0"))), //requires D 1.0
  //
  //      V("C")("v" -> Set("2.0"))(),
  //      V("C")("v" -> Set("3.0"))(),
  //
  //      V("D")("v" -> Set("2.0"))(
  //        X("C")("v" -> Set("2.0"))),
  //      V("D")("v" -> Set("1.0"))(
  //        X("C")("v" -> Set("3.0"))), //requires C 3.0
  //
  //      V("B")("v" -> Set("1.0"))(
  //        X("C")("v" -> Set("2.0")),
  //        X("F")()),
  //      V("B")("v" -> Set("2.0"))(
  //        X("C")("v" -> Set("3.0")), //requires C 3.0
  //        X("F")()),
  //      V("F")("v" -> Set("1.0"))(
  //        X("C")("v" -> Set("2.0"))),
  //      V("F")("v" -> Set("2.0"))(
  //        X("C")("v" -> Set("3.0"))))) //requires C 3.0
  //
  //    val state = resolved(result)
  //    println(state)
  //    checkResolved(state, Set("A", "B", "C", "D", "E", "F"))
  //    checkUnresolved(state, Set())
  //    checkVariants(state, "A" -> ("v" -> Set("1.0")))
  //    checkVariants(state, "B" -> ("v" -> Set("2.0")))
  //    checkVariants(state, "C" -> ("v" -> Set("3.0")))
  //    checkVariants(state, "D" -> ("v" -> Set("1.0")))
  //    checkVariants(state, "E" -> ("v" -> Set("1.0")))
  //    checkVariants(state, "F" -> ("v" -> Set("2.0")))
  //  }
  //
  //  test("basic under-constrained path find") {
  //    val result = load(useTestData(
  //      R("A")("v" -> Set("1.0"))(
  //        X("B")(),
  //        X("C")("v" -> Set("2.0"))),
  //
  //      V("C")("v" -> Set("2.0"))(),
  //      V("C")("v" -> Set("3.0"))(),
  //
  //      V("B")("v" -> Set("1.0"))(
  //        X("C")("v" -> Set("2.0"))),
  //      V("B")("v" -> Set("2.0"))(
  //        X("C")("v" -> Set("3.0")))))
  //
  //    println(result)
  //    val state = resolved(result)
  //    checkResolved(state, Set("A", "B", "C"))
  //    checkVariants(state, "A" -> ("v" -> Set("1.0")))
  //    checkVariants(state, "B" -> ("v" -> Set("1.0")))
  //    checkVariants(state, "C" -> ("v" -> Set("2.0")))
  //    checkUnresolved(state, Set())
  //  }
  //
  //  test("multiple under-constrained paths find") {
  //    val result = load(useTestData(
  //      R("A")("v" -> Set("1.0"))(
  //        X("B")(),
  //        X("C")(),
  //        X("D")(),
  //        X("E")("v" -> Set("2.0"))),
  //
  //      V("E")("v" -> Set("2.0"))(),
  //      V("E")("v" -> Set("3.0"))(),
  //
  //      V("B")("v" -> Set("1.0"))(
  //        X("E")("v" -> Set("2.0"))),
  //      V("B")("v" -> Set("2.0"))(
  //        X("E")("v" -> Set("3.0"))),
  //
  //      V("C")("v" -> Set("1.0"))(
  //        X("E")("v" -> Set("2.0"))),
  //      V("C")("v" -> Set("2.0"))(
  //        X("E")("v" -> Set("3.0"))),
  //
  //      V("D")("v" -> Set("1.0"))(
  //        X("E")("v" -> Set("2.0"))),
  //      V("D")("v" -> Set("2.0"))(
  //        X("E")("v" -> Set("3.0")))))
  //
  //    println(result)
  //    val state = resolved(result)
  //
  //    checkResolved(state, Set("A", "B", "C", "D", "E"))
  //    checkVariants(state, "A" -> ("v" -> Set("1.0")))
  //    checkVariants(state, "B" -> ("v" -> Set("1.0")))
  //    checkVariants(state, "C" -> ("v" -> Set("1.0")))
  //    checkVariants(state, "D" -> ("v" -> Set("1.0")))
  //    checkVariants(state, "E" -> ("v" -> Set("2.0")))
  //    checkUnresolved(state, Set())
  //  }
  //
  //  test("larger unconstrained graphs") {
  //    val (dependencies, variants) = useTestData(LargeDataSets.basic: _*)
  //
  //    val result = new Resolver(new DefinedVariants(variants)).resolve(Set(
  //      // uncomment to resolve:
  //      //            Dependency(Id("org.scala-lang/scala-library"), Set(Constraint("version", Set("2.10.3")))),
  //      //            Dependency(Id("com.typesafe.akka/akka-actors"),  Set(Constraint("version", Set("2.2.0")))),
  //      Dependency(Id("com.typesafe.play/play"), Set.empty),
  //      Dependency(Id("com.typesafe.play/play-slick"), Set.empty)))
  //
  //    val state = unresolved(result)
  //    result match {
  //      case result: UnderconstrainedResult => result.optimalStates must have size 8
  //      case _ => assert(false, "result was not under constrained: " + result)
  //    }
  //  }

}

