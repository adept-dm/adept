package adept.cli.commands.module

import adept.cli.commands.Command
import adept.models._

object ModuleAddDependencyCommand extends Command {

  val command = "add-dependency"
  val shortDescription = "add artifact to current module"

  def execute(args: List[String]): CommandResult = {
    Left("yeah")
  }

  def parseArgs(args: List[String]) = {
    val (coordsStr :: hash :: config :: Nil) = args

    for {
      coords <- Coordinates.parse(coordsStr).right
    } yield {
      (coords, hash, config)
    }
  }
}
