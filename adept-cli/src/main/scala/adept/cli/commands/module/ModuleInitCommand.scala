package adept.cli.commands.module

import adept.cli.commands.Command
import adept.core.models._

object ModuleInitCommand extends Command with JsonFileSystemModulePersistance {

  val command = "init"
  val shortDescription = "initiate publishing of new module"
  override val help = Some("args: org:name:version")

  override def execute(args: List[String]): CommandResult = {
    for {
      module <- parseArgs(args).right
      _ <- persistModule(module).right
    } yield { None }
  }

  def parseArgs(args: List[String]): Either[String, Module] = {
    if (args.length == 1) {
      for {
        coords <- Coordinates.parse(args.head).right
      } yield {
        Module(coords, Set(), Set(), Map(), Set())
      }
    }
    else {
      Left(help.get)
    }
  }
}
