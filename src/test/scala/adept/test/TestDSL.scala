package adept.test

import adept.core.models._
import adept.ext.Query
import scala.collection.immutable.Queue

/**
 * TestDSL used to create visual hierarchies that are easy to test
 *
 * See example usage below:
 * {{{
 * import TestDSL._
 * useTestData(
 * R("A")("version" -> "V")( //add a variant and a root dependency
 * X("B")("version" -> "X")), //add a dependency on B version X
 *
 * V("B")("version" -> "X")()) //create a variant (not added as a dependency)
 *
 * //or
 * useTestData(
 * R("A")("version" -> "V")( //add a variant and a root dependency
 * V("B")("version" -> "X")( //add a variant and add as dependendency to parent
 * V("C")("version" -> "Y")( //add anoter variant and add as dependendency to parent
 * )),
 *
 * V("B")("version" -> "Z")()) //create a variant (not added as a dependency, because no parent)
 *
 * }}}
 */
object TestDSL {
  sealed trait TestType
  sealed class BuildableTestType(val id: String, val attrs: Seq[(String, Set[String])], val children: Seq[TestType]) extends TestType

  /** V for variant is a dependency to its parents and also added to tree */
  class V(id: String)(attrs: Seq[(String, Set[String])])(children: Seq[TestType]) extends BuildableTestType(id, attrs, children)
  object V {
    def apply(id: String)(attrs: (String, Set[String])*)(children: TestType*) = new V(id)(attrs)(children)
  }

  /** R for root is like C but also returns the dependency to the variant */
  class R(id: String)(attrs: Seq[(String, Set[String])])(children: Seq[TestType]) extends BuildableTestType(id, attrs, children)
  object R {
    def apply(id: String)(attrs: (String, Set[String])*)(children: TestType*) = new R(id)(attrs)(children)
  }

  /** define only dependency */
  class X(val id: String)(val constraints: Seq[(String, Set[String])]) extends TestType
  object X {
    def apply(id: String)(constraints: (String, Set[String])*) = new X(id)(constraints)
  }

  def toDependency(v: BuildableTestType): Dependency = {
    Dependency(new Id(v.id), constraints = v.attrs.map { case (name, values) => Constraint(name, values) }.toSet)
  }

  implicit def exclusionType[A <: BuildableTestType](testType: A) = {
    new {
      def exclude(query: Query)(implicit exclusions: collection.mutable.Set[Dependency]): A = {
        exclusions += toDependency(testType)
        testType
      }
    }
  }

  implicit def overridesType[A <: BuildableTestType](testType: A) = {
    new {
      def overrides(query: Query, attrs: (String, (String, Set[String]))*)(implicit overrides: collection.mutable.Set[(Dependency, Map[String, Set[Attribute]])]): A = {
        val dependency = toDependency(testType)
        overrides += dependency -> attrs.map{ case (id, (name, values)) => id -> Set(Attribute(name, values)) }.toMap
        testType
      }
    }
  }

  def useTestData(data: TestType*): (Set[Dependency], Seq[Variant]) = {

    var dependencies = Queue.empty[Dependency]
    var variants = Queue.empty[Variant]

    def build[A](parent: BuildableTestType, testTypes: Seq[TestType]): Dependency = {
      val deps = testTypes map {
        case r: R => throw new Exception("Found a R (for Root): " + r + " in: " + testTypes + ". Use V (for Variant) instead!")
        case d: X => Dependency(new Id(d.id), constraints = d.constraints.map { case (name, values) => Constraint(name, values) }.toSet)
        case v: V => build(v, v.children)
        case other => throw new Exception("Found a " + other + "  in: " + testTypes + ". Use V (for Variant) instead!")
      }
      val parentVariant = Variant(new Id(parent.id), artifacts = Set.empty, attributes = parent.attrs.map { case (name, values) => Attribute(name, values) }.toSet, dependencies = deps.toSet)

      variants :+= parentVariant

      toDependency(parent)
    }

    data foreach {
      case r: R => {
        val rootDep = toDependency(r)

        dependencies :+= rootDep

        build(r, r.children)
      }
      case v: V => build(v, v.children)
      case d: X => throw new Exception("Found a X (a Dependency): " + d + " in: " + data + ". Use V (for Variant) or R (for Root) or instead!")
      case other => throw new Exception("Found a " + other + "  in: " + data + ". Use V (for Variant) or R (for Root) or instead!")
    }

    (dependencies.toSet, variants)
  }
}