package adept.cli.commands

import adept.core._
import java.io.File
import adept.core.parsers.Parsers
import adept.core.models._
import adept.core.ivy.IvyHelpers



object IvyCommand extends Command {
  override val command = "ivy-set"
  override def description = """set a dependency from ivy"""
  /*override def help = s"""
    |usage: adept $command --server=<hostname> <repository name> <coordinates> <jar file>
    |       adept $command <repository name> <coordinates> <jar file> <jar url>
    |       adept $command <repository name> <coordinates> <hash>
    """.stripMargin*/

  case class IvyConfig(
      coordsString: String = null, 
      ivySettingsString: String = null)
  
  val simpleParser = new scopt.immutable.OptionParser[IvyConfig](s"adept $command", Commands.Version) { def options = Seq(
    arg("<coords>", "coordinates") { (v: String, c: IvyConfig) => c.copy(coordsString = v) },
    opt("s", "ivy-settings", "path to ivy settings") { (v: String, c: IvyConfig) => c.copy(ivySettingsString = v) }
  )}
  
  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName
    val dir = Configuration.currentAdeptDir()
    val ivyConf = adept.core.Configuration.defaultIvyConf
    
    simpleParser.parse(args, IvyConfig()) map { config =>
      (for {
        ivy <- IvyHelpers.load(Option(config.ivySettingsString)).right
        coords <- Parsers.coords(config.coordsString).right
      } yield {
        IvyHelpers.set(coords, ivy, ivyConf, Adept(dir, repoName)): Either[String, Seq[Module]]
      }).joinRight.right.map(_.mkString("\n"))
    } getOrElse {
      Left("could not find the correct arguments and options")
    }
  }
}