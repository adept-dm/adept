package adept.cli

import adept.cli.commands.Command

object CliHelpers {

  def help(error: Option[String], commands: Map[String, Command]) = error.getOrElse("")+"""
  |usage: adept <command> <options>
  |commands:
  |""".stripMargin+{
    val maxLength = commands.map(_._1).maxBy(_.length).length
    commands.map(c => c._1 + "   " + " "*(maxLength - c._1.length) + c._2.shortDescription).mkString("   ", "\n   ", "")}
  
  
  def commandParser(args: List[String], commands: Map[String, Command]) = {
    if (args.headOption == Some("-h") || args.headOption == Some("--help"))
      println(help(None, commands))
    else
      args match {  
        case command :: commandArgs =>
          commands.get(command) match {
            case Some(command) =>
              command.execute(commandArgs)
              .left.map{ error => 
                println("Error: "+ error)
              }.right.foreach{ msg => 
                println(msg)
              }
            case _ => println(help(Some("unknown command: '"+command+"'"), commands))
          }
        case noCommand => {
          println(help(Some("no command specified!"), commands))
        }
    }
  }
}
