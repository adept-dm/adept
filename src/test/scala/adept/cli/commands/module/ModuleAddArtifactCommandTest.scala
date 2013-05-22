package adept.cli.commands.module 

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

import adept.models._

class ModuleAddArtifactCommandTest extends FunSuite with MustMatchers {

  test("parse simple") {
    ModuleAddArtifactCommand.parseArgs(
      List("someType", "someloc", "--", "somecfg")) must be === ("someType", Set("someloc"), Set("somecfg"))
  }

  test("parse no cfg") {
    ModuleAddArtifactCommand.parseArgs(
      List("sometype", "someloc", "anotherloc")) must be === ("sometype", Set("someloc", "anotherloc"), Set())
  }

  test("parse no locs") {
    ModuleAddArtifactCommand.parseArgs (
      List("sometype", "--", "cfg1", "cfg2")) must be === ("sometype", Set(), Set("cfg1", "cfg2"))
  }

  val module = Module(Coordinates("a", "b", "c"), Set(), Set(
    Configuration("c1", None, Set(), Visibility.Public, None),
    Configuration("c2", None, Set(), Visibility.Public, None)
  ), Map(), Set())

  test("checkConfigs pass all") {
    ModuleAddArtifactCommand.checkConfigs(Set("c2", "c1"), module) must be === None
  }

  test("checkConfigs pass one") {
    ModuleAddArtifactCommand.checkConfigs(Set("c2"), module) must be === None
  }

  test("checkConfigs fail") {
    ModuleAddArtifactCommand.checkConfigs(
      Set("c1", "c3", "c2"), module) must be === Some("configuration c3 is not a member of current module")
  }


}
