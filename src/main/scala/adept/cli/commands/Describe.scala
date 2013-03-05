package adept.cli.commands

import adept.core._

object DescribeCommand extends Command {
  override val command = "describe"
  override def description = """describes the dependencies matching the input"""
  override def help = s"""
    |usage: adept $command <coordinate[<metadata>]>
    """.stripMargin
  override def execute(args: List[String]): Either[String, String] = {
    val coordsArg: Either[String, String] = if (args.size > 1)
      Left("too many coordinates for describe")
    else {
      args.headOption.toRight("please provide coordinates")
    }

    (for {
      a <- coordsArg.right
      (coords, meta) <- Parsers.coordsMetadata(a).right
    } yield {
      Adept.describe(coords, meta)(db.database).right.map{ case (parent, children) => 
        (parent +: children).mkString("\n")
      }
    }).joinRight
  }
}
