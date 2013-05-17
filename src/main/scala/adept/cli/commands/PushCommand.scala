package adept.cli.commands

import adept.models.Coordinates
import adept.models.Configuration
import adept.models.Visibility
import adept.models.Artifact
import adept.utils.Logging



object PushCommand extends Command with Logging {

  override val command = "push"
  override val shortDescription = "push a jar into repository"

  override def execute(args: List[String]): Either[String, String] = {
    Right("hi")
  }

  def processArgs(args: List[String]):
      Either[String, (Coordinates, Configuration, List[String])] = {
    val (optional, required) = args.partition(_.startsWith("-"))
    val optionalSet = optional.toSet

    val coordsString :: reqTail = required
    val artifacts = if (reqTail.length == 0) { 
      Left("at least one artifact must be specified")
    } else {
      Right(reqTail)
    }

    for {
      arts <- artifacts.right
      coords <- Coordinates.parse(coordsString).right
      config <- buildConfiguration(optionalSet).right
    } yield {
      logger.info("pusing %s %s %s" format (coords, config, arts))
      (coords, config, arts)
    }
  }

  def buildConfiguration(params: Set[String]): Either[String, Configuration] = {
    val paramMap = params.map(_.splitAt('=')).toMap
    Right(Configuration("", None, Set(), Visibility.Public , None))
  }

//  def publish(coor: Coordinates, artifacts: List[String], config: Configuration )

  val help = 
"""
usage: adept push org:name:version artifact1 artifact2*

you can also set optional parameters by using --paramter_name=paramter_value syntax
"""

}
