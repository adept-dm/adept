package adept.cli.commands

import adept.core._
import java.io.File
import adept.core.parsers.Parsers
import adept.core.models._

object SetCommand extends Command {
  override val command = "set"
  override def description = """update or add a dependency"""
  /*override def help = s"""
    |usage: adept $command --server=<hostname> <repository name> <coordinates> <jar file>
    |       adept $command <repository name> <coordinates> <jar file> <jar url>
    |       adept $command <repository name> <coordinates> <hash>
    """.stripMargin*/
  
  override def help = s"""
    |usage: adept $command <coordinates> <jar file>
    """.stripMargin
    
  case class SimpleConfig(
      coordsString: String = null, 
      jarFileString: String = null)
  
  val simpleParser = new scopt.immutable.OptionParser[SimpleConfig](s"adept $command", Commands.Version) { def options = Seq(
    arg("<coords>", "coordinates") { (v: String, c: SimpleConfig) => c.copy(coordsString = v) },
    arg("<jar file>", "jar file path") { (v: String, c: SimpleConfig) => c.copy(jarFileString = v) }
  ) }
  
  override def execute(args: List[String]): Either[String, String] = {
    val dir = new File(Configuration.currentAdeptDir(), Configuration.defaultRepoName)
    
    simpleParser.parse(args, SimpleConfig()) map { config =>
      (for {
        coordsMetadata <- Parsers.coordsMetadata(config.coordsString).right
        jarFile <- { 
          val path = config.jarFileString
          val jarFile = new File(path)
          if (jarFile.exists && jarFile.isFile) {
            Right(jarFile)
          } else Left(s"Could not find a file for path: $path")
        }.right
      } yield {
        val (coords, metadata) = coordsMetadata
        val module = Module.fromFile(jarFile, coords, metadata, Set.empty)
        Adept(dir)
          .set(module)
          .toOption
          .toRight(s"could not set module: $module").right
          .map{ _ => module.toString }
      }).joinRight
    } getOrElse {
      Left("could not find the correct arguments and options")
    }
  }
}