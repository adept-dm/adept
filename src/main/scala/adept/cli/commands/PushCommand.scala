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
    val (optional, required) = args.span(_.startsWith("-"))
    val optionalSet = optional.toSet

    val coordsString :: artifacts = required

    for {
      coords <- Coordinates.parse(coordsString).right
      config <- buildConfiguration(optionalSet).right
      artifacts <- buildArtifacts(artifacts).right
    } yield {
      logger.info("pusing %s %s %s" format (coords, config, artifacts))
      "win"
    }

  }

  def buildArtifacts(artifacts: List[String]): Either[String, List[Artifact]] = {
    Right(List())
  }


  def buildConfiguration(params: Set[String]): Either[String, Configuration] = {
    Right(Configuration("", None, Set(), Visibility.Public , None))
  }

//  def publish(coor: Coordinates, artifacts: List[String], config: Configuration )

  val help = 
"""
usage: adept push org:name:version artifact1 artifact2*

you can also set optional parameters by using --paramter_name=paramter_value syntax
"""

}




