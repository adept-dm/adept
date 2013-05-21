package adept.cli.commands

import adept.models.Coordinates
import adept.models.Configuration
import adept.models.Visibility
import adept.models.Artifact
import adept.utils.Logging



object PublishCommand extends Command with Logging {

  override val command = "push"
  override val shortDescription = "push a jar into repository"

  val allowedOptions = Set("description", "extends-from", "visibility", "depreatecd")

  override def execute(args: List[String]): CommandResult = {
    Left("just so")
  }

  def publish(coords: Coordinates, config: Configuration, arts: List[String]) = {
    
  }

  def processArgs(args: List[String]):
      Either[String, (Coordinates, Configuration, List[String])] = {
    val (optional, required) = args.partition(_.startsWith("-"))
    val optionalSet = optional.toSet

    val coordsString :: reqTail = required
    val artifacts = if ( reqTail.isEmpty ) {
      Left("at least one artifact must be specified")
    } else {
      Right(reqTail)
    }

    for {
      arts <- artifacts.right
      coords <- Coordinates.parse(coordsString).right
      config <- buildConfiguration(coords.name, optionalSet).right
    } yield {
      logger.info("pusing %s %s %s" format (coords, config, arts))
      (coords, config, arts)
    }
  }

  def buildConfiguration(name: String, params: Set[String]): Either[String, Configuration] = {
    val paramMap: Map[String, String] = params.map { str =>
      val param = str.dropWhile(_ == '-')
      val (x1,x2) = param.span(_ != '=')
      (x1,x2.drop(1)) //drop = at the beginning of x2
    } toMap

    for {
      visibility <- parseVisibility(paramMap.get("visibility").getOrElse("public")).right
    } yield {
      Configuration(
        name,
        paramMap.get("description"),
        paramMap.get("extends-from").map(_.split(',').toSet).getOrElse(Set()),
        visibility,
        paramMap.get("depreceated"))
    }
  }

  def parseVisibility(str: String): Either[String, Visibility.Value] = {
    val lower = str.toLowerCase
    if(lower == "private") {
      Right(Visibility.Private)
    } else if(lower == "public") {
      Right(Visibility.Public)
    } else {
      Left("visibility must be either 'public' or 'private'")
    }
  }



//  def publish(coor: Coordinates, artifacts: List[String], config: Configuration )

  val help =
"""
usage: adept push org:name:version artifact1 artifact2*

you can also set optional parameters by using --paramter_name=paramter_value syntax
"""

}
