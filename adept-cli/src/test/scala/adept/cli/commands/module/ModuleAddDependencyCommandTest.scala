package adept.cli.commands.module

import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import adept.core.models._
import adept.core.models.UniqueId

class ModuleAddDependencyCommandTest extends FunSuite with MustMatchers {

  test("parseArgs pass") {
    ModuleAddDependencyCommand.parseArgs(
      List("a:b:c", "unique tasting brownies", "config")) must be === Right(
      Dependency(Coordinates("a", "b", "c"), Some(UniqueId("unique tasting brownies")), "config"))
  }

  test("parseArgs fail") {
    ModuleAddDependencyCommand.parseArgs(
      List("a:b:c", "unique", "config")) must be === Right(
      Dependency(Coordinates("a", "b", "c"), Some(UniqueId("unique")), "config"))
  }

}
