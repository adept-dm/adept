package adept.cli.commands

import adept.core._
import java.io.File

object CommitCommand extends Command {
  
  override val command = "commit"
  override def description = """commit current changes"""
   
  override def help = s"""
    |usage: adept $command
    """.stripMargin
 
  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName //TODO: repoName as a arg
    val dir = Configuration.currentAdeptDir()
    val a = Adept(dir, repoName).commit
    if (a.isSuccess) Right(a.get.toString)
    else Left(a.failed.get.getMessage) 
  }
}