package adept.core.operations

import org.scalatest._
import adept.core.operations._
import adept.core.models._

class TreeTest extends FunSuite with MustMatchers {
  import adept.core.tests.TestData._
  
  test("basic tree") {
    val tree = Tree.build("test", adept10, Configuration.defaultConfigurationMapping(_), findModule)
    Tree.evict(tree, extralib, "parce que je le vaux bien")
    println(tree)
  }
}