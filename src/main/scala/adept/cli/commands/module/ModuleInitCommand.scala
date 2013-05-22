package adept.cli.commands.module

import adept.cli.commands.Command
import adept.models._

object ModuleInitCommand extends Command {

  val command = "init"
  val shortDescription = "initiate publishing of new module"

  override def execute(args: List[String]): CommandResult = {
    Left("blah")
  }

  def parseArgs(args: List[String]): Either[String, Module] = {
    val (coordsString :: configVals) = args
    for {
      coords <- Coordinates.parse(coordsString).right
    } yield {
      Module(coords, Set(), Set(), Map(), Set())
    }
  }

}
