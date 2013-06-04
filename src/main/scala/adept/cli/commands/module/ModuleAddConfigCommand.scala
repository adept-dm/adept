package adept.cli.commands.module

import adept.cli.commands.Command
import adept.models._

object ModuleAddConfigCommand extends Command with JsonFileSystemModulePersistance {

  val command = "add-config"
  val shortDescription = "add configuration"
  override val help = Some("usage: name --visibility={public,private} --description=yourdescription --extends-from=extendsfrom --depreceated=depreceated")

  def execute(args: List[String]): CommandResult = {
    def addCfgToModule(cfg: Configuration)(module: Module): Module = {
      print(module)
      val newModule = module.copy(
        configurations = module.configurations + cfg
      )
      print(newModule)
      newModule
    }

    for {
      cfg <- parseArgs(args).right
      result <- updatePersistedModule(addCfgToModule(cfg)).right
    } yield {
      None
    }

  }
    def parseArgs(args: List[String]) = {
    val (name :: configStrings) = args
    for {
      config <- parseConfiguration(name, configStrings.toSet).right
    } yield {
      config
    }
  }

  def parseConfiguration(name: String, params: Set[String]): Either[String, Configuration] = {
    val paramMap: Map[String, String] = params.map { str =>
      val param = str.dropWhile(_ == '-')
      val (x1,x2) = param.span(_ != '=')
      (x1,x2.drop(1)) //drop = at the beginning of x2
    } toMap

    for {
      visibility <- parseVisibility(paramMap.get("visibility").getOrElse("public")).right
    } yield {
      Configuration(
        name,
        paramMap.get("description"),
        paramMap.get("extends-from").map(_.split(',').toSet).getOrElse(Set()),
        visibility,
        paramMap.get("deprecated"))
    }
  }

  def parseVisibility(str: String): Either[String, Visibility.Value] = {
    val lower = str.toLowerCase
    if(lower == "private") {
      Right(Visibility.Private)
    } else if(lower == "public") {
      Right(Visibility.Public)
    } else {
      Left("visibility must be either 'public' or 'private', not %s" format (str))
    }
  }
}
