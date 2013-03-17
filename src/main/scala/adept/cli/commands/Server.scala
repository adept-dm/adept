package adept.cli.commands

import adept.core._
import java.io.{ File => jFile }

object InitCommand extends Command {
  
  override val command = "server"
  override def description = """start serving adept from this folder"""
  
    
  override def help = s"""
    |usage: adept $command
    """.stripMargin
    
 
  override def execute(args: List[String]): Either[String, String] = {
    val dir = new jFile(Configuration.currentAdeptDir(), Configuration.defaultRepoName)
    val a = Adept(dir)
    a.server
    Right("adept started!")
  }
}