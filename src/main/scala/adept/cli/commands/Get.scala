package adept.cli.commands

import adept.core._
import java.io.File
import adept.core.parsers.Parsers
import adept.core.models._

object GetCommand extends Command {
  override val command = "get"
  override def description = """get all dependencies"""

  case class GetConfig(
      coordsString: String = null)
  
  val simpleParser = new scopt.immutable.OptionParser[GetConfig](s"adept $command", Commands.Version) { def options = Seq(
    arg("<coords>", "coordinates") { (v: String, c: GetConfig) => c.copy(coordsString = v) }
  ) }
  
  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName
    val dir = Configuration.currentAdeptDir()
    
    simpleParser.parse(args, GetConfig()) map { config =>
      (for {
        coords <- Parsers.coords(config.coordsString).right
      } yield {
        Adept(dir, repoName)
          .get(coords).map(_.mkString("\n")): Either[String, String]
      }).joinRight
    } getOrElse {
      Left("could not find the correct arguments and options")
    }
  }
}