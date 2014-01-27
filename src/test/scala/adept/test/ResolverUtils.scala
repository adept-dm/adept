package adept.test

import adept.models._
import adept.resolution._
import adept.resolution.models._
import adept.repository._
import org.scalatest.matchers.MustMatchers

object ResolverUtils extends MustMatchers {

  implicit def stringToId(id: String): Id = {
    Id(id)
  }

  implicit def tuple2ToAttribute(t: (String, Set[String])): Attribute = {
    val (name, values) = t
    Attribute(name, values)
  }

  implicit def tuple2ToConstraint(t: (String, Set[String])): Constraint = {
    val (name, values) = t
    Constraint(name, values)
  }

  implicit def tuple2ToRequirement(t: (String, Set[Constraint])): Requirement = {
    val (name, constraints) = t
    Requirement(name, constraints)
  }

  def checkResolved(result: ResolveResult, ids: Set[Id]) = {
    val state = result.state
    (state.resolved ++ state.implicitVariants.keys) must equal(ids)

  }

  def checkUnresolved(result: ResolveResult, ids: Set[Id]) = {
    val state = result.state
    val underconstrained = state.underconstrained
    val overconstrained = state.overconstrained
    (overconstrained ++ underconstrained) must equal(ids)
  }

  def checkVariants(result: ResolveResult, id: Id, attr: Attribute) = {
    import org.scalatest.OptionValues._
    assert(result.state.isResolved, "Could not find resolved state:\n" + result)
    val state = result.state
    val variant = (state.resolvedVariants ++ state.implicitVariants).get(id).value
    variant.id must equal(id)
    (id -> variant.attributes) must equal(id -> Set(attr))
  }

  def getResolver(variants: Set[Variant]): Resolver = {
    val memVariantsLoader = new MemoryVariantsLoader(variants)
    new Resolver(memVariantsLoader)
  }

  def resolve(requirements: Set[Requirement], variants: Set[Variant]): ResolveResult = {
    val result = getResolver(variants)
    result.resolve(requirements)
  }
}