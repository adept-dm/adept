package adept.client

import adept.repository._
import java.io.{File => jFile}

object Commands {
  def all: Map[String, Command] = Seq(
      InitCommand,
      AddCommand,
      DescribeCommand).map(c => c.command -> c).toMap
}

trait Command {
  val command: String
  def execute(args: List[String]): Either[String, String] 
  def description: String
  def help: String
}

object AddCommand extends Command {
  override val command = "add"
  override def description = """adds a dependency to current repository"""
  override def help = s"""
    |usage: adept $command <options> <repository name> <coordinates> <jar file>
    """.stripMargin
  override def execute(args: List[String]): Either[String, String] = {
    val repoDir = Configuration.currentAdeptDir()
    
    val repoArg = args.drop(0).take(1).headOption
    val coordsArg = args.drop(1).take(1).headOption
    val jarArg = args.drop(2).take(1).headOption
    
    (for {
      repoName <- repoArg.toRight("please provide a repository name").right
      coordsAndMetadataString <- coordsArg.toRight("please provide coordinates").right
      jar <- jarArg.toRight("please provide a jar file").right
      jarFile <- jarArg.map(f => new jFile(f)).filter(_.exists).toRight(s"jar file: '$jar' does not exist").right
      (coords, metadata) <- Parsers.coordsMetadata(coordsAndMetadataString).right
    } yield {
      val hash = Hash.calculate(coords, jarFile)
      val descriptor = Descriptor(coords, metadata, hash)
      Repository.add(descriptor, jarFile).right.map(_.toString)
    }).joinRight
  }
}

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
      Repository.describe(coords, meta).right.map(_.toString)
    }).joinRight
  }
}

object InitCommand extends Command {
  override val command = "init"
  override def description = """initialises current directory with a new repository"""
  override def help = s"""
    |usage: adept $command
    """.stripMargin
  override def execute(args: List[String]): Either[String, String]  = {
    val dir = Configuration.currentAdeptDir()
    Repository.init(dir)
    Right(s"Initialized adept in $dir")
  }
}