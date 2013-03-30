package adept.cli.commands

import adept.core._
import java.io.File

object InitCommand extends Command {
  
  override val command = "init"
  override def description = """init adept here"""
    
  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName //TODO: repoName as a arg
    val dir = new File(Configuration.currentAdeptDir(), repoName)
    val a = Adept.init(dir, repoName)
    a.map(_ => ("initialized in: " + dir.toString) )
  }
}