package adept.cli.commands.module

import adept.cli.commands.Command
import adept.models._

object ModuleAddArtifactCommand extends Command {

  val command = "add-artifact"
  val shortDescription = "add artifact to current module"

  def execute(args: List[String]): CommandResult = {
    Left("yeah")
  }

  def parseArgs(args: List[String]) = {
    val (artifactType :: tail) = args
    val (locations, configz) = tail.span( _ != "--")
    val configs = if (configz.length > 0) { configz.tail } else { Nil }

    (artifactType, locations.toSet, configs.toSet)
  }

  def checkConfigs(configs: Set[String], module: Module): Option[String] = {
    if( configs.isEmpty) {
      None
    } else {
      val head = configs.head
      if ( module.configurations.find(_.name == head).isEmpty ) {
        Some("configuration %s is not a member of current module" format (head))
      } else {
        checkConfigs(configs.tail, module)
      }
    }
  }
}
