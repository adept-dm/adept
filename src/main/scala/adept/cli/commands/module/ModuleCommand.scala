package adept.cli.commands.module

import adept.cli.CliHelpers
import adept.cli.commands.Command
import adept.utils.Logging

object ModuleCommand extends Command with Logging {

  override val command = "module"
  override val shortDescription = "module command dispatcher"

  def moduleCommands: Map[String, Command] = CliHelpers.all(
    ModuleInitCommand,
    ModuleAddArtifactCommand,
    ModuleAddConfigCommand,
    ModulePublishCommand
  )

  override def execute(args: List[String]): CommandResult = {
    CliHelpers.commandParser(args, moduleCommands)
  }

}
