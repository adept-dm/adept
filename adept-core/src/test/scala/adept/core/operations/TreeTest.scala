package adept.core.operations

import org.scalatest._
import adept.core.operations._
import adept.core.models._

class TreeTest extends FunSuite with MustMatchers {
  import adept.core.tests.TestData._
  
  test("simple tree test") {
    import org.scalatest.EitherValues._
    println(TreeOperations.build("test", adept10.dependencies, adept10.universes, adept10.configurations, configMapping, findModule(modules) _))
    val tree = TreeOperations.build("test", adept10.dependencies, adept10.universes, adept10.configurations, configMapping, findModule(modules) _).right.value
    ConflictResolver.resolveConflicts(tree, Configuration.defaultConfigurationMapping(_), findModule(modules))
    println(tree)
    pending
  }

  test("intransitive tree test") {
    import org.scalatest.EitherValues._
    val tree = TreeOperations.build("test", adept10Intransitive.dependencies, adept10Intransitive.universes, adept10Intransitive.configurations, configMapping, findModule(modules) _).right.value
    ConflictResolver.resolveConflicts(tree, Configuration.defaultConfigurationMapping(_), findModule(modules))
    println(tree)
    pending
  }
}