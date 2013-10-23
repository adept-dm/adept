package adept.ext

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.test.TestDSL._
import adept.test.TestHelpers._
import adept.core.models._
import adept.core.resolution.Resolver
import adept.test.DefinedVariants

class ExclusionsTest extends FunSuite with MustMatchers {

  test("attribute merging") {
    val result = Extensions.mergeAttributes(Set(Attribute("foo", Set("bar", "baz")), Attribute("arg", Set("boo"))), Set(Attribute("foo", Set("bar", "loo")), Attribute("zoo", Set("bear"))))
    result must be === Set(Attribute("foo", Set("bar", "baz", "loo")), Attribute("arg", Set("boo")), Attribute("zoo", Set("bear")))
  }

  test("variant basic hashing") {
    val variantA1 = Variant("A", Set(Artifact("123", Set(Attribute("master", Set("compile"))))), Set(Attribute("version", Set("A"))), Set(Dependency("B", Set(Constraint("version", Set("X"))))))
    val variantA2 = Variant("A", Set(Artifact("123", Set(Attribute("master", Set("compile"))))), Set(Attribute("version", Set("A"))), Set(Dependency("B", Set(Constraint("version", Set("X"))))))
    //minor variations:
    val variantA3 = Variant("A", Set(Artifact("12", Set(Attribute("master", Set("compile"))))), Set(Attribute("version", Set("A"))), Set(Dependency("B", Set(Constraint("version", Set("X"))))))
    val variantA4 = Variant("A", Set(Artifact("123", Set(Attribute("maste", Set("compile"))))), Set(Attribute("version", Set("A"))), Set(Dependency("B", Set(Constraint("version", Set("X"))))))
    val variantA5 = Variant("A", Set(Artifact("123", Set(Attribute("master", Set("compile"))))), Set(Attribute("version", Set("1A"))), Set(Dependency("B", Set(Constraint("version", Set("X"))))))
    val variantA6 = Variant("A", Set(Artifact("123", Set(Attribute("master", Set("compile"))))), Set(Attribute("version", Set("A"))), Set(Dependency("B", Set(Constraint("version", Set("X1"))))))
    val variantA7 = Variant("A", Set(Artifact("123", Set(Attribute("master", Set("compile"))))), Set(Attribute("version", Set("A"))), Set(Dependency("B", Set(Constraint("version1", Set("X"))))))
    val variantA8 = Variant("A", Set(Artifact("123", Set(Attribute("master", Set("compile1"))))), Set(Attribute("version", Set("A"))), Set(Dependency("B", Set(Constraint("version", Set("X"))))))
    val variantB = Variant("B", Set(Artifact("123", Set(Attribute("master", Set("compile"))))), Set(Attribute("version", Set("A"))), Set(Dependency("B", Set(Constraint("version", Set("X"))))))
    
    Hash.calculate(variantA1) must equal(Hash.calculate(variantA2))
    Hash.calculate(variantA1) must not equal (Hash.calculate(variantA3))
    Hash.calculate(variantA1) must not equal (Hash.calculate(variantA4))
    Hash.calculate(variantA1) must not equal (Hash.calculate(variantA5))
    Hash.calculate(variantA1) must not equal (Hash.calculate(variantA6))
    Hash.calculate(variantA1) must not equal (Hash.calculate(variantA7))
    Hash.calculate(variantA1) must not equal (Hash.calculate(variantA8))
    Hash.calculate(variantA1) must not equal (Hash.calculate(variantB))
  }

  test("end-to-end exclusions") {
    val exclusionQuery = Query(("organization" -> "boo.foo")) //<-- boo.foo
    //This is graph is unresolved, because C is over-constrained,
    //although it would be preferable to override here, imagine that we choose to exclude
    //variants of C (organization boo.foo) from the transitive graph of A (means exclude from B)
    implicit val exclusions = collection.mutable.Set.empty[Dependency]
    val (dependencies, variants) = useTestData(
      R("A")("v" -> Set("V"), "organization" -> Set("foo.com"), "name" -> Set("A"), "exclusions" -> Set.empty)(
        X("B")("v" -> Set("X"))) exclude(exclusionQuery),
      V("B")("v" -> Set("X"), "organization" -> Set("org.loo"), "name" -> Set("B"), "exclusions" -> Set("E:123aef"))(
        X("C")("v" -> Set("X"))),
      V("C")("v" -> Set("X"), "organization" -> Set("boo.foo"), "name" -> Set("C"), "exclusions" -> Set.empty)( //<-- boo.foo
        X("D")()),
      V("C")("v" -> Set("Y"), "organization" -> Set("boo.foo"), "name" -> Set("C"), "exclusions" -> Set.empty)(
        X("D")()),
      R("C")("v" -> Set("Z"), "organization" -> Set("boo.foo"), "name" -> Set("C"), "exclusions" -> Set.empty)(
        X("D")()),
      V("D")("v" -> Set("X"), "organization" -> Set("bar"), "name" -> Set("D"), "exclusions" -> Set.empty)())

    val excludeResolver = new Resolver(new DefinedVariants(variants))
    println("PRE-EXCLUSION RESULT: " + excludeResolver.resolve(dependencies))

    //we resolve from A (the ones we want to exclude)
    val excludeResolveResult = excludeResolver.resolve(exclusions.toSet)
    val excludeState = resolved(excludeResolveResult)
    unresolved(excludeResolver.resolve(dependencies)) //check that this is really unresolved
    
    //we find which new variants which needs to be created and...
    //...which dependencies we need add (since we are adding new variants)... 
    val excludeVariants = excludeState.implicitVariants ++ excludeState.resolvedVariants
//    val exclusionResult = Extensions.exclude(dependencies, excludeState.graph, excludeVariants, 
//        query = exclusionQuery)
//    val newDependencies = exclusionResult.dependencies
//    val newVariants = exclusionResult.newVariants
//
//    import OptionValues._
//
//    newVariants must have size(1)
//    val bVariant = newVariants.headOption.value
//    bVariant.id must be === "B"
//    bVariant.attributes.filter(_.name == "exclusions").flatMap(_.values) must have size(2)
//    
//    val newIds = newVariants.map {
//      case variant =>
//        variant.id -> variant.dependencies.map(_.id)
//    }
//
//    newIds must be === Set(
//      "B" -> Set())
//    
//    val resolver = new Resolver(new DefinedVariants(variants ++ newVariants))
//
//    val result = resolver.resolve(newDependencies) //resolving again, but specifying which exclusion we want
//    println("FINAL RESULT: " + result)
//    val state = resolved(result)
//    state.implicitVariants must be('empty) //excluded constraints are added so resolution in this case should not be forced
//    state.resolvedVariants.collect{ case (id, variant) if id == "B" => variant }.toSet must be === newVariants
  }

}