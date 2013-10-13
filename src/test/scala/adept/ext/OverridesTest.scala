package adept.ext

import org.scalatest._
import org.scalatest.matchers.MustMatchers
import adept.test.TestDSL._
import adept.test.TestHelpers._
import adept.core.models._
import adept.core.resolution.Resolver
import adept.test.DefinedVariants

class OverridesTest extends FunSuite with MustMatchers {

  test("end-to-end overrides") {
    val overrideQuery = Query(("name" -> "C"))
    
    //This graph is over-constrained because B depends on C v X,
    //and also there is a dependency on C v Z
    //by so we construct a query that finds C under A (B is under A)
    //and create a new variant of this one that uses C v Z instead
    implicit val overrides = collection.mutable.Set.empty[(Dependency, Map[String, Set[Attribute]])]
    val (dependencies, variants) = useTestData(
      R("A")("v" -> Set("V"), "organization" -> Set("foo.com"), "name" -> Set("A"), "overrides" -> Set.empty, "exclusions" -> Set.empty)(
        X("B")("v" -> Set("X"))) overrides(overrideQuery, "C" -> ("v" -> Set("Z"))),
      V("B")("v" -> Set("X"), "organization" -> Set("org.loo"), "name" -> Set("B"), "overrides" -> Set("E:123aef:abc2456"), "exclusions" -> Set("E:123aef"))(
        X("C")("v" -> Set("X"))),
      V("C")("v" -> Set("X"), "organization" -> Set("boo.foo"), "name" -> Set("C"), "overrides" -> Set.empty, "exclusions" -> Set.empty)(
        X("D")()),
      V("C")("v" -> Set("Y"), "organization" -> Set("boo.foo"), "name" -> Set("C"), "overrides" -> Set.empty, "exclusions" -> Set.empty)(
        X("D")()),
      R("C")("v" -> Set("Z"), "organization" -> Set("boo.foo"), "name" -> Set("C"), "overrides" -> Set.empty, "exclusions" -> Set.empty)(
        X("D")()),
      V("D")("v" -> Set("X"), "organization" -> Set("bar"), "name" -> Set("D"), "overrides" -> Set.empty, "exclusions" -> Set.empty)())

    val overridesResolver = new Resolver(new DefinedVariants(variants))
    println("PRE-EXCLUSION RESULT: " + overridesResolver.resolve(dependencies))

    //we resolve from A (the base we want to override from)
    val overridesResolveResult = overridesResolver.resolve(overrides.map(_._1).toSet)
    val overridesState = resolved(overridesResolveResult)
    unresolved(overridesResolver.resolve(dependencies)) //check that this is really unresolved
    
    val overridesVariants = overridesState.forcedVariants ++ overridesState.resolvedVariants
    val replacements = overrides.foldLeft(Map.empty[String, Set[Attribute]])(_ ++ _._2)
    val overrideResult = Extensions.overrides(dependencies, overridesState.graph, overridesVariants, 
        query = overrideQuery, replacements)
    val newDependencies = overrideResult.dependencies
    val newVariants = overrideResult.newVariants
    
    
    import OptionValues._

    newVariants must have size(1)
    val bVariant = newVariants.headOption.value
    bVariant.moduleId must be === "B"
    bVariant.attributes.filter(_.name == "overrides").flatMap(_.values) must have size(2)
    
    val newIds = newVariants.map {
      case variant =>
        variant.moduleId -> variant.dependencies.map(_.id)
    }

    newIds must be === Set(
      "B" -> Set("C"))
    
    val resolver = new Resolver(new DefinedVariants(variants ++ newVariants))

    val result = resolver.resolve(newDependencies) //resolving again, but specifying which exclusion we want
    println("FINAL RESULT: " + result)
    val state = resolved(result)
    state.forcedVariants must be('empty) //excluded constraints are added so resolution in this case should not be forced
    state.resolvedVariants.collect{ case (id, variant) if id == "B" => variant }.toSet must be === newVariants
  }

}