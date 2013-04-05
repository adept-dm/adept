package adept.cli.commands

import adept.core._
import java.io.File

object PullCommand extends Command {
  
  override val command = "pull"
  override def description = """download and merge data from remote repository"""
    
  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName //TODO: repoName as a arg
    val host = "localhost"
    val port = 1337
    val timeout = Configuration.defaultTimeout
    
    val dir = Configuration.currentAdeptDir()
    val a = Adept(dir, repoName)
    
    a.pull(timeout).map(hash => s"updated $repoName to $hash")
  }
}