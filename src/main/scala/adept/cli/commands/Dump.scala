package adept.cli.commands

import adept.core._
import java.io.File

object DumpCommand extends Command {
  
  override val command = "dump"
  override def description = """dimp"""
    
  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName //TODO: repoName as a arg
    
    val dir = Configuration.currentAdeptDir()
    val a = Adept(dir, repoName)
    
    Right(a.dump.map{ case (module, commit, deleted) => s"$module-$commit-(deleted: $deleted)"}.mkString("\n") )
  }
}