package adept.test

import adept.core.models._
import adept.core.resolution._
import org.scalatest.matchers.MustMatchers

object TestHelpers extends MustMatchers {

  def load(testData: (Seq[Dependency], Seq[Variant])): Resolver = {
    val (dependencies, all) = testData

    val resolver = new Resolver(new DefinedVariants(all))
    resolver.resolve(dependencies)
    resolver
  }

  def checkUnresolved(resolver: Resolver, ids: Set[String]) = {
    resolver.state.unresolved must equal(ids)
  }

  def checkResolved(resolver: Resolver, ids: Set[String]) = {
    resolver.state.resolved must equal(ids)
  }
  
  def checkConstraints(resolver: Resolver, attr: (String, (String, String))) = {
    val (id, (attrName, attrValue)) = attr
    import org.scalatest.OptionValues._
    resolver.state.globalConstraints.get(id).value must equal(Constraint(attrName, Set(attrValue)))
  }
  
   
  def checkVariants(resolver: Resolver, attr: (String, (String, String))) = {
    val (id, (attrName, attrValue)) = attr
    import org.scalatest.OptionValues._
    val variants = resolver.state.allVariants.get(id).value 
    variants must have size(1)
    val variant = variants.headOption.value
    variant.moduleId must equal(id)
    variant.attributes must equal(Set(Attribute(attrName, Set(attrValue))))
  }
}


object VariantsLoaderEngineTester extends VariantsLoaderLogic with MustMatchers {
  def testMatches(attributes: Set[Attribute], constraints: Set[Constraint], expectMatchValue: Boolean) = {
    assert(matches(attributes, constraints) == expectMatchValue, "expected attributes: " + attributes + " constraints: " + constraints + " to " + (if (expectMatchValue) "" else "NOT") + " match")
  }
}