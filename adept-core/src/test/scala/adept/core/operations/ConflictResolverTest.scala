package adept.core.operations


import org.scalatest._
import adept.core.operations._
import adept.core.models._

class ConflictResolverTest extends FunSuite with MustMatchers {
  import adept.core.tests.TestData._
  
  test("basic conflict resolution"){
    import org.scalatest.OptionValues._
    val tree = TreeOperations.build("test", adept10, Configuration.defaultConfigurationMapping(_), findModule).value
    ConflictResolver.resolveConflicts(tree, Configuration.defaultConfigurationMapping(_), findModule)
    println(tree)
    pending
  }
}