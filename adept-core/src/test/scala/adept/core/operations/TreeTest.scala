package adept.core.operations

import org.scalatest._
import adept.core.operations._
import adept.core.models._

class TreeTest extends FunSuite with MustMatchers {
  import adept.core.tests.TestData._
  
  test("simple tree test") {
    import org.scalatest.OptionValues._
    val tree = TreeOperations.build("test", adept10, Configuration.defaultConfigurationMapping(_), findModule(modules)).value
    ConflictResolver.resolveConflicts(tree, Configuration.defaultConfigurationMapping(_), findModule(modules))
    println(tree)
    pending
  }

  test("intransitive tree test") {
    import org.scalatest.OptionValues._
    val tree = TreeOperations.build("test", adept10Intransitive, Configuration.defaultConfigurationMapping(_), findModule(modules)).value
    ConflictResolver.resolveConflicts(tree, Configuration.defaultConfigurationMapping(_), findModule(modules))
    println(tree)
    pending
  }
}