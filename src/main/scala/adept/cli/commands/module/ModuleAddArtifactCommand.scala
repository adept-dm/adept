package adept.cli.commands.module

import adept.cli.commands.Command

object ModuleAddArtifactCommand extends Command {

  val command = "add-artifact"
  val shortDescription = "add artifact to current module"

  def execute(args: List[String]): CommandResult = {
    Right(Some("some"))
  }

}
