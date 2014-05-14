package adept.resolution

import adept.resolution.models._
import adept.repository.AttributeConstraintFilter
import org.scalatest._

class MatcherTest extends FunSuite {
  def testMatches(attributes: Set[Attribute], constraints: Set[Constraint], expectMatchValue: Boolean) = {
    assert(AttributeConstraintFilter.matches(attributes, constraints) == expectMatchValue, "expected attributes: " + attributes + " and constraints: " + constraints + " to " + (if (expectMatchValue) "" else "NOT") + " match")
  }
//
//  test("one to one matching") {
//    testMatches(Set(Attribute("version", Set("1.0"))), Set(Constraint("version", Set("1.0"))), true)
//  }
//
//  test("multi attributes with one matching constraint") {
//    testMatches(Set(Attribute("version", Set("1.0")), Attribute("binary-version", Set("1.0"))), Set(Constraint("version", Set("1.0"))), true)
//  }
//
//  test("one to one with no matching constraints") {
//    testMatches(Set(Attribute("version", Set("1.0"))), Set(Constraint("nomatch", Set("1.0"))), false)
//  }
//
//  test("multiple constraints, one not matching") {
//    testMatches(Set(Attribute("version", Set("1.0"))), Set(Constraint("version", Set("1.0")), Constraint("binary-version", Set("1.0"))), false)
//  }
//
//  test("multiple attributes and constraints matching") {
//    testMatches(Set(Attribute("version", Set("1.0")), Attribute("binary-version", Set("1.0"))), Set(Constraint("version", Set("1.0")), Constraint("binary-version", Set("1.0"))), true)
//  }
//
//  test("constraint conflict (no matches possible)") {
//    testMatches(Set(Attribute("version", Set("1.0"))), Set(Constraint("version", Set("1.0")), Constraint("version", Set("2.0"))), false)
//  }
//
//  test("constraints that requires nothing to be set") {
//    testMatches(Set(Attribute("version", Set("1.0")), Attribute("excludes", Set())), Set(Constraint("version", Set("1.0")), Constraint("excludes", Set())), true)
//  }
//
//  test("configurations verifications") {
//    testMatches(Set(Attribute("configuration", Set("1.0")), Attribute("excludes", Set())), Set(Constraint("version", Set("1.0")), Constraint("excludes", Set())), true)
//  }
//
  test("that java architecture can be represented by attributes/constraints") {
    testMatches(Set(Attribute("binary-version", Set("1.5", "1.4", "1.7", "1.6", "1.1", "1.2", "1.3"))), Set(Constraint("binary-version", Set("1.5")), Constraint("binary-version", Set("1.4"))), true)
  }
}
