package adept.cli.commands.module

import adept.Adept
import adept.cli.commands.Command

object ModulePublishCommand extends Command with JsonFileSystemModulePersistance {

  val command = "publish"
  val shortDescription = "publish current module to the world"

  def execute(args: List[String]): CommandResult = {
    for {
      adept <- Adept.open(new java.io.File(".adept"), "local").right
      module <- loadModule.right
    } yield  {
      adept.add(module)
      adept.commit("published")
      adept.push("git@github.com:tomasherman/adept-central.git")
      None
    }
  }
}
