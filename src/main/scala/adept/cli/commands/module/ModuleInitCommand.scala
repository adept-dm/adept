package adept.cli.commands.module

import adept.cli.commands.Command

object ModuleInitCommand extends Command {

  val command = "init"
  val shortDescription = "initiate publishing of new module"

  override def execute(args: List[String]): CommandResult = {
    Right(Some("init"))
  }

}
