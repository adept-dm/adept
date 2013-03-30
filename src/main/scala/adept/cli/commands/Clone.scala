package adept.cli.commands

import adept.core._
import java.io.File

object CloneCommand extends Command {
  
  override val command = "clone"
  override def description = """clone from one directory to another"""

  case class CloneConfig(
      fromDirString: String = null)
  
  val simpleParser = new scopt.immutable.OptionParser[CloneConfig](s"adept $command", Commands.Version) { def options = Seq(
    arg("<dir>", "from directory") { (v: String, c: CloneConfig) => c.copy(fromDirString = v) }
  ) }
  
  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName //TODO: repoName as a arg
    val dir = Configuration.currentAdeptDir()
    simpleParser.parse(args, CloneConfig()).map{ config =>
      val fromDir = new File(new File(config.fromDirString), Configuration.adeptDir)
      if (fromDir.exists && fromDir.isDirectory) {
        val result = Adept.clone(dir, fromDir, repoName)
        result.map(r => "cloned with hash " + r): Either[String, String]
      } else{
        Left("could not find an existing directory in: " + fromDir.getAbsolutePath)
      }
    }.getOrElse{
      Left("could not find the correct arguments and options")
    }
  }
}