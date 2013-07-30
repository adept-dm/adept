package adept.cli.commands.module

import org.scalatest.FunSuite
import org.scalatest.MustMatchers

import adept.core.models._

class ModuleInitCommandTest extends FunSuite with MustMatchers {
  val coordsStr = "org:name:ver"
  val coordsParsed = Coordinates("org", "name", "ver")


  test("test basic processing") {
    import org.scalatest.EitherValues._
    
    val parsed = ModuleInitCommand.parseArgs(List(coordsStr)).right.value
    
    parsed.coordinates === coordsParsed
  }

}
