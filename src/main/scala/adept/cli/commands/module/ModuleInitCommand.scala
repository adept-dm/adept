package adept.cli.commands.module

import adept.cli.commands.Command
import adept.models._

object ModuleInitCommand extends Command with JsonFileSystemModulePersistance {

  val command = "init"
  val shortDescription = "initiate publishing of new module"

  override def execute(args: List[String]): CommandResult = {
    for {
      module <- parseArgs(args).right
      _ <- persistModule(module).right
    } yield { None }
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
