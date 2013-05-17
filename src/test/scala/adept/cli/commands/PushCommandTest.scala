package adept.cli.commands

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

import adept.models._

class PushCommandTest extends FunSuite with MustMatchers {

  test("test basic processing") {
    val parsed = PushCommand.processArgs(List("org:name:ver", "some.jar"))
    parsed must be === Right((
      Coordinates("org", "name", "ver"),
      Configuration("", None, Set(), Visibility.Public , None),
      List("some.jar")
    ))
  }

  test("fail no artifacts") {
    val parsed = PushCommand.processArgs(List("org:name:ver"))
    parsed must be === Left("at least one artifact must be specified")
  }

}
