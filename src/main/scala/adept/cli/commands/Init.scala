package adept.cli.commands

import adept.core._
import java.io.File

object ServerCommand extends Command {
  
  override val command = "init"
  override def description = """init adept here"""
   
  override def help = s"""
    |usage: adept $command
    """.stripMargin
    
 
  override def execute(args: List[String]): Either[String, String] = {
    val dir = new File(Configuration.currentAdeptDir(), Configuration.defaultRepoName)
    val a = Adept.init(dir)
    if (a.isSuccess) Right("initialized in: " + dir.toString)
    else Left(a.failed.get.getMessage) 
  }
}