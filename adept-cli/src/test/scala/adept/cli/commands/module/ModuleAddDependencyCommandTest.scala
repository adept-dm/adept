package adept.cli.commands.module

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

import adept.core.models._

class ModuleAddDependencyCommandTest extends FunSuite with MustMatchers {

  test("parseArgs pass") {
    ModuleAddDependencyCommand.parseArgs(
      List("a:b:c", "hash brownies", "config")) must be === Right(
      Dependency(Coordinates("a", "b", "c"), Hash("hash brownies"), "config"))
  }

  test("parseArgs fail") {
    ModuleAddDependencyCommand.parseArgs(
      List("a:b:c", "hash", "config")) must be === Right(
      Dependency(Coordinates("a", "b", "c"), Hash("hash"), "config"))
  }

}
