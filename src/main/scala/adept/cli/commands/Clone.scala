package adept.cli.commands

import adept.core._
import java.io.File

object CloneCommand extends Command {
  
  override val command = "clone"
  override def description = """clone an adept repository"""

  case class CloneConfig(
      path: String = null)
  
  val simpleParser = new scopt.immutable.OptionParser[CloneConfig](s"adept $command", Commands.Version) { def options = Seq(
    arg("<path>", "path to local directory or remote server") { (v: String, c: CloneConfig) => c.copy(path = v) }
  ) }
  
  
  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName //TODO: repoName as a arg
    val dir = Configuration.currentAdeptDir()
    simpleParser.parse(args, CloneConfig()).map{ config =>
      val result = Adept.clone(dir, config.path, repoName)
      result.map(r => "cloned with hash " + r): Either[String, String]
    }.getOrElse{
      Left("could not find the correct arguments and options")
    }
  }
}