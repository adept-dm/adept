package adept.cli.commands

import adept.core._
import java.io.File

object ServerCommand extends Command {
  
  override val command = "server"
  override def description = """start serving adept from this folder"""

  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName
    val dir = Configuration.currentAdeptDir()
    val a = Adept(dir, repoName)
    a.server
    Right(s"serving $repoName...")
  }
}