package adept.resolution

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.resolution.models._
import adept.repository._

class ResolverTest extends FunSuite with MustMatchers {
  import adept.test.ResolverUtils._

  test("Very simple resolution works correctly") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("V")),
        requirements = Set("B" -> Set.empty[Constraint])),

      Variant("B", Set(version -> Set("X")),
        requirements = Set("B" -> Set.empty[Constraint])))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(version, Set("V"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A", "B"))
    checkVariants(result, "A", version -> Set("V"))
    checkVariants(result, "B", version -> Set("X"))
  }

  // FIXME:
  //  test("Internal combinations method works as expected") {
  //    val d10 = Variant("D", Set(version -> Set("1.0")))
  //    val d20 = Variant("D", Set(version -> Set("2.0")))
  //    val e10 = Variant("E", Set(version -> Set("1.0")))
  //
  //    val variants: Set[Variant] = Set(
  //      Variant("A", Set(version -> Set("1.0"))),
  //      Variant("A", Set(version -> Set("2.0"))),
  //      Variant("B", Set(version -> Set("1.0"))),
  //      Variant("B", Set(version -> Set("2.0"))),
  //      Variant("C", Set(version -> Set("1.0"))),
  //      Variant("C", Set(version -> Set("2.0"))),
  //      d10,
  //      d20,
  //      e10)
  //
  //    val resolver = getResolver(variants)
  //
  //    val combinations = resolver.combinations(Set(new Id("D"), new Id("E")), Set.empty, Map.empty).map(_.toSet).toList
  //    combinations(0) must be === Set(List(d10), List(d20), List(e10))
  //
  //    //FIXME: this one should not fail if I am right?
  //    //D 1.0, E 1.0, D 2.0, D 1.0 & E 1.0, D 2.0 & E 1.0
  //    combinations(1) must be === Set(List(d10, e10), List(d20, e10))
  //  }

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

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A", "B", "C", "D"))
    checkVariants(result, "D", version -> Set("Z"))
  }

  test("Simple under-constrained results behaves correctly") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("B" -> Set[Constraint](binaryVersion -> Set("1.0")))),

      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0"))),
      Variant("B", Set(version -> Set("1.0.1"), binaryVersion -> Set("1.0"))))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(binaryVersion, Set("1.0"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A"))
    checkUnresolved(result, Set("B"))
  }

  test("basic over-constrained") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("B" -> Set[Constraint](binaryVersion -> Set("XXX")))),

      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0"))),
      Variant("B", Set(version -> Set("1.0.1"), binaryVersion -> Set("1.0"))))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(binaryVersion, Set("1.0"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A"))
    checkUnresolved(result, Set("B"))
  }

  test("transitive, multi variant") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("B" -> Set[Constraint]())),
      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("C" -> Set[Constraint]())),
      Variant("C", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("D" -> Set[Constraint]())),
      Variant("D", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set()))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(binaryVersion, Set("1.0"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A", "B", "C", "D"))
    checkUnresolved(result, Set())
  }

  test("basic cyclic dependencies") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("V")),
        requirements = Set("B" -> Set.empty[Constraint])),

      Variant("B", Set(version -> Set("X")),
        requirements = Set("A" -> Set[Constraint](version -> Set("V")))))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(version, Set("V"))),
      "B" -> Set(Constraint(version, Set("X"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A", "B"))
    checkUnresolved(result, Set())
  }

  test("cyclic dependencies, multiple variants possible") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("V")),
        requirements = Set("B" -> Set[Constraint](version -> Set("Y")))),

      Variant("B", Set(version -> Set("Y")),
        requirements = Set("B" -> Set[Constraint]())),

      Variant("A", Set(version -> Set("X")),
        requirements = Set("C" -> Set[Constraint](version -> Set("Z"))))) //does not exist

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(version, Set("V"))),
      "B" -> Set(Constraint(version, Set("Y"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A", "B"))
    checkUnresolved(result, Set())
  }

  test("nested constraints") {
    //B is unconstrained, but D forces C v 3.0, only B v 1.0 is constrained on C v 3.0 so B v 1.0 must be used:
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set(
          "B" -> Set[Constraint](),
          "C" -> Set[Constraint](),
          "D" -> Set[Constraint](),
          "E" -> Set[Constraint]())),

      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("C" -> Set[Constraint](binaryVersion -> Set("2.0")))),
      Variant("B", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set("C" -> Set[Constraint](binaryVersion -> Set("3.0")))),

      Variant("C", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set()),
      Variant("C", Set(version -> Set("3.0.0"), binaryVersion -> Set("3.0")),
        requirements = Set()),

      Variant("D", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set("C" -> Set[Constraint](binaryVersion -> Set("2.0")))),
      Variant("D", Set(version -> Set("2.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("C" -> Set[Constraint](binaryVersion -> Set("3.0")))),

      Variant("E", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set(
          "D" -> Set[Constraint](binaryVersion -> Set("1.0")),
          "B" -> Set[Constraint]())))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(binaryVersion, Set("1.0"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A", "B", "C", "D", "E"))
    checkUnresolved(result, Set())
  }

  test("nested under-constrained path find") {
    //B is under-constrained initially and so is F, but since E requires D v 1.0
    //and D 1.0 requires C 3.0, only B 2.0 and F 2.0 can be used with C 3.0
    //this graph should be resolved    val variants: Set[Variant] = Set(
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set(
          "B" -> Set[Constraint](),
          "C" -> Set[Constraint](),
          "D" -> Set[Constraint](),
          "E" -> Set[Constraint]())),

      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set(
          "C" -> Set[Constraint](binaryVersion -> Set("2.0")),
          "F" -> Set[Constraint]())),
      Variant("B", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set(
          "C" -> Set[Constraint](binaryVersion -> Set("3.0")), //requires C 3.0
          "F" -> Set[Constraint]())),

      Variant("C", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set()),
      Variant("C", Set(version -> Set("3.0.0"), binaryVersion -> Set("3.0")),
        requirements = Set()),

      Variant("D", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set("C" -> Set[Constraint](binaryVersion -> Set("2.0")))),
      Variant("D", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("C" -> Set[Constraint](binaryVersion -> Set("3.0")))), //requires C 3.0

      Variant("E", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("D" -> Set[Constraint](binaryVersion -> Set("1.0")))), //requires D 1.0

      Variant("F", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("C" -> Set[Constraint](binaryVersion -> Set("2.0")))),
      Variant("F", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set("C" -> Set[Constraint](binaryVersion -> Set("3.0"))))) //requires C 3.0

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(binaryVersion, Set("1.0"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A", "B", "C", "D", "E", "F"))
    checkUnresolved(result, Set())
  }

  test("basic under-constrained path find") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set(
          "B" -> Set[Constraint](),
          "C" -> Set[Constraint](binaryVersion -> Set("2.0")))),

      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set(
          "C" -> Set[Constraint](binaryVersion -> Set("2.0")))),
      Variant("B", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set(
          "C" -> Set[Constraint](binaryVersion -> Set("3.0")))),

      Variant("C", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set()),
      Variant("C", Set(version -> Set("3.0.0"), binaryVersion -> Set("3.0")),
        requirements = Set()))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(binaryVersion, Set("1.0"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A", "B", "C"))
    checkUnresolved(result, Set())
    checkVariants(result, "A", version -> Set("1.0.0"), binaryVersion -> Set("1.0"))
    checkVariants(result, "B", version -> Set("1.0.0"), binaryVersion -> Set("1.0"))
    checkVariants(result, "C", version -> Set("2.0.0"), binaryVersion -> Set("2.0"))
  }

  test("multiple under-constrained paths find") {
    val variants: Set[Variant] = Set(

      Variant("A", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set(
          "B" -> Set[Constraint](),
          "C" -> Set[Constraint](),
          "D" -> Set[Constraint](),
          "E" -> Set[Constraint](binaryVersion -> Set("2.0")))),

      Variant("B", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("E" -> Set[Constraint](binaryVersion -> Set("2.0")))),
      Variant("B", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set("E" -> Set[Constraint](binaryVersion -> Set("3.0")))),

      Variant("C", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("E" -> Set[Constraint](binaryVersion -> Set("2.0")))),
      Variant("C", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set("E" -> Set[Constraint](binaryVersion -> Set("3.0")))),

      Variant("D", Set(version -> Set("1.0.0"), binaryVersion -> Set("1.0")),
        requirements = Set("E" -> Set[Constraint](binaryVersion -> Set("2.0")))),
      Variant("D", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set("E" -> Set[Constraint](binaryVersion -> Set("3.0")))),

      Variant("E", Set(version -> Set("2.0.0"), binaryVersion -> Set("2.0")),
        requirements = Set()),
      Variant("E", Set(version -> Set("3.0.0"), binaryVersion -> Set("3.0")),
        requirements = Set()))

    val requirements: Set[Requirement] = Set(
      "A" -> Set(Constraint(binaryVersion, Set("1.0"))))

    val result = resolve(requirements, getMemoryLoader(variants))
    checkResolved(result, Set("A", "B", "C", "D", "E"))
    checkUnresolved(result, Set())
    checkVariants(result, "A", version -> Set("1.0.0"), binaryVersion -> Set("1.0"))
    checkVariants(result, "B", version -> Set("1.0.0"), binaryVersion -> Set("1.0"))
    checkVariants(result, "C", version -> Set("1.0.0"), binaryVersion -> Set("1.0"))
    checkVariants(result, "D", version -> Set("1.0.0"), binaryVersion -> Set("1.0"))
    checkVariants(result, "E", version -> Set("2.0.0"), binaryVersion -> Set("2.0"))
  }

  //TODO:
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

