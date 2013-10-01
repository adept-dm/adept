package adept.test

import adept.core.models._
import adept.core.resolution._
import org.scalatest.matchers.MustMatchers

object TestHelpers extends MustMatchers {

  def load(testData: (Seq[Dependency], Seq[Variant])): Either[State, State] = {
    val (dependencies, all) = testData

    val resolver = new Resolver(new DefinedVariants(all))
    resolver.resolve(dependencies)
  }

  def resolved(state: Either[State, State]): State = {
    assert(state.isRight, "could not find resolved state:\n" + state)
    state.right.get
  }
  
  def unresolved(state: Either[State, State]): State = {
    assert(state.isLeft, "could not find unresolved state:\n" + state)
    state.left.get
  }
  
  def checkUnresolved(state: State, ids: Set[String]) = {
    val underconstrained = state.underconstrained
    val overconstrained = state.overconstrained
    (overconstrained ++ underconstrained) must equal(ids)
  }

  def checkResolved(state: State, ids: Set[String]) = {
    state.resolved ++ state.forcedVariants.keys must equal(ids)
  }
  
  def checkConstraints(state: State, attr: (String, (String, String))) = {
    val (id, (attrName, attrValue)) = attr
    import org.scalatest.OptionValues._
    state.constraints.get(id).value must equal(Constraint(attrName, Set(attrValue)))
  }
  
   
  def checkVariants(state: State, attr: (String, (String, String))) = {
    val (id, (attrName, attrValue)) = attr
    import org.scalatest.OptionValues._
    val variant = (state.resolvedVariants ++ state.forcedVariants).get(id).value 
    variant.moduleId must equal(id)
    variant.attributes must equal(Set(Attribute(attrName, Set(attrValue))))
  }
}


object VariantsLoaderEngineTester extends VariantsLoaderLogic with MustMatchers {
  def testMatches(attributes: Set[Attribute], constraints: Set[Constraint], expectMatchValue: Boolean) = {
    assert(matches(attributes, constraints) == expectMatchValue, "expected attributes: " + attributes + " constraints: " + constraints + " to " + (if (expectMatchValue) "" else "NOT") + " match")
  }
}