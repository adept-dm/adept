package adept.cli.commands.module

import adept.cli.commands.Command

object ModulePublishCommand extends Command {

  val command = "publish"
  val shortDescription = "publish current module to the world"

  def execute(args: List[String]): CommandResult = {
    Right(Option("publish"))
  }
}
