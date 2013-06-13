package adept.cli

import adept.cli.commands.Command

object CliHelpers {

  def all(commands: Command*): Map[String, Command] = {
    commands.map(c => (c.command, c)).toMap
  }

  def help(error: Option[String], commands: Map[String, Command]) = error.getOrElse("")+"""
  |usage: adept <command> <options>
  |commands:
  |""".stripMargin+{
    val maxLength = commands.map(_._1).maxBy(_.length).length
    sortedCommands(commands).map(c => c._1 + "   " + " "*(maxLength - c._1.length) + c._2.shortDescription).mkString("   ", "\n   ", "")}

  private def sortedCommands(commands: Map[String, Command]) = {
    commands.toSeq.sortBy(_._1)
  }

  def commandParser(args: List[String], commands: Map[String, Command]): Either[String, Option[String]] = {
    if (args.headOption == Some("-h") || args.headOption == Some("--help"))
      Right(Some(help(None, commands)))
    else
      args match {
        case command :: commandArgs =>
          commands.get(command) match {
            case Some(command) => command.executeWithHelpCheck(commandArgs)
            case _ => Left(help(Some("unknown command: '"+command+"'"), commands))
          }
        case noCommand => {
          Left(help(Some("no command specified!"), commands))
        }
    }
  }
}
