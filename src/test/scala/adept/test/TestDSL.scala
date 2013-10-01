package adept.test

import adept.core.models._

/**
  TestDSL used to create visual hierarchies that are easy to test

  See example usage below:
 {{{
   import TestDSL._
   useTestData(
    R("A")("version" -> "V")( //add a variant and a root dependency
      X("B")("version" -> "X")), //add a dependency on B version X
  
    V("B")("version" -> "X")()) //create a variant (not added as a dependency)
    
   //or
   useTestData(
    R("A")("version" -> "V")( //add a variant and a root dependency
      V("B")("version" -> "X")( //add a variant and add as dependendency to parent
        V("C")("version" -> "Y")( //add anoter variant and add as dependendency to parent
      )),
  
    V("B")("version" -> "Z")()) //create a variant (not added as a dependency, because no parent)
    
 }}}
 */
object TestDSL {
  sealed trait TestType
  sealed class BuildableTestType(val id: String, val attrs: Seq[(String, String)], val children: Seq[TestType]) extends TestType

  /** V for variant is a dependency to its parents and also added to tree */
  class V(id: String)(attrs: Seq[(String, String)])(children: Seq[TestType]) extends BuildableTestType(id, attrs, children)
  object V {
    def apply(id: String)(attrs: (String, String)*)(children: TestType*) = new V(id)(attrs)(children)
  }

  /** R for root is like C but also returns the dependency to the variant */
  class R(id: String)(attrs: Seq[(String, String)])(children: Seq[TestType]) extends BuildableTestType(id, attrs, children)
  object R {
    def apply(id: String)(attrs: (String, String)*)(children: TestType*) = new R(id)(attrs)(children)
  }

  /** define only dependency */
  class X(val id: String)(val constraints: Seq[(String, String)]) extends TestType
  object X {
    def apply(id: String)(constraints: (String, String)*) = new X(id)(constraints)
  }

  def toDependency(v: BuildableTestType): Dependency = {
    Dependency(v.id, constraints = v.attrs.map { case (name, value) => Constraint(name, Set(value)) }.toSet)
  }

  def useTestData(data: TestType*): (Set[Dependency], Seq[Variant]) = {
    val dependencies = new collection.mutable.Queue[Dependency]
    val variants = new collection.mutable.Queue[Variant]

    def build(parent: BuildableTestType, testTypes: Seq[TestType]): Dependency = {
      val deps = testTypes map {
        case r: R => throw new Exception("Found a R (for Root): " + r + " in: " + testTypes + ". Use V (for Variant) instead!")
        case d: X => Dependency(d.id, constraints = d.constraints.map { case (name, value) => Constraint(name, Set(value)) }.toSet)
        case v: V => build(v, v.children)
        case other => throw new Exception("Found a " + other + "  in: " + testTypes + ". Use V (for Variant) instead!")
      }
      val parentVariant = Variant(parent.id, artifacts = Set.empty, attributes = parent.attrs.map { case (name, value) => Attribute(name, Set(value)) }.toSet, dependencies = deps.toSet)

      //MUTATE!
      parentVariant +=: variants

      toDependency(parent)
    }

    data foreach {
      case r: R => {
        val rootDep = toDependency(r)

        //MUTATE!
        rootDep +=: dependencies

        build(r, r.children)
      }
      case v: V => build(v, v.children)
      case d: X => throw new Exception("Found a X (dependency): " + d + " in root data: " + data + ". Use V (for Variant) or R (for root) instead!")
      case other => throw new Exception("Found a " + other + "  in: " + data + ". Use V (for Variant) or R (for Root) or instead!")
    }

    (dependencies.toSet, variants)
  }
}