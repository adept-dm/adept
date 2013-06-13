package adept.cli.commands.module

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

import adept.models._

class ModuleAddConfigCommandTest extends FunSuite with MustMatchers {

  test("test processing with params") {
    val descr = "somedescription"
    val extendsFrom = "projectA,projectB,projectC"
    val depreceated = "indeeddepreceated"
    val parsed = ModuleAddConfigCommand.parseArgs(List(
      "datname",
      "--description=" + descr,
      "--extends-from=" + extendsFrom,
      "--visibility=private",
      "--deprecated=" + depreceated))

    parsed must be === Right(
      Configuration(
        "datname",
        Some(descr),
        extendsFrom.split(",").toSet,
        Visibility.Private,
        Some(depreceated))
    )
  }

}
