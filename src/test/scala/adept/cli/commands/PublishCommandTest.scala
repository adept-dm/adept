package adept.cli.commands

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

import adept.models._

class PublishCommandTest extends FunSuite with MustMatchers {
  val coordsStr = "org:name:ver"
  val coordsParsed = Coordinates("org", "name", "ver")


  test("test basic processing") {
    val parsed = PublishCommand.processArgs(List(coordsStr, "some.jar"))
    parsed must be === Right((
      coordsParsed,
      Configuration("name", None, Set(), Visibility.Public , None),
      List("some.jar")
    ))
  }

  test("fail no artifacts") {
    val parsed = PublishCommand.processArgs(List(coordsStr))
    parsed must be === Left("at least one artifact must be specified")
  }

  test("test processing with params") {
    val descr = "somedescription"
    val extendsFrom = "projectA,projectB,projectC"
    val depreceated = "indeeddepreceated"
    val parsed = PublishCommand.processArgs(List(
      coordsStr,
      "--description=" + descr,
      "--extends-from=" + extendsFrom,
      "--visibility=private",
      "--depreceated=" + depreceated,
      "some.jar"))

    parsed must be === Right((
      coordsParsed,
      Configuration(
        "name", Some(descr),
        extendsFrom.split(",").toSet,
        Visibility.Private,
        Some(depreceated)),
      List("some.jar"))
    )
  }

}
