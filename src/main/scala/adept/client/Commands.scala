package adept.client

import adept.repository._
import java.io.{File => jFile}

object Commands {
  def all: Map[String, Command] = Seq(
      InitCommand,
      AddCommand,
      ListCommand,
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
    def parseInputDescriptors: Either[String, List[Descriptor]] = {
      val reader = {
        import java.io._
        new BufferedReader(new InputStreamReader(System.in));
      }
      @annotation.tailrec def slurpInput(lines: List[String]): List[String] = {
        val line = reader.readLine()
        if (line != null && line.nonEmpty) slurpInput(line :: lines)
        else lines
      }
      slurpInput(List.empty).foldLeft(Right(List.empty): Either[String, List[Descriptor]]){ 
        (current, string) => {
          for {
            currentDescriptors <- current.right
            descriptor <- Parsers.descriptor(string).right
          } yield {
            descriptor +: currentDescriptors 
          }
        }
      }
    }
    
    //TODO: output should not be done here!
    val cmdLineMsgPrintStream = System.err
    def whileWaiting[T](msg: String)(block: => T) = {
      cmdLineMsgPrintStream.print(msg)
      try {
        block
      } finally {
        cmdLineMsgPrintStream.print("\r"*(msg.size)) 
      }
    }
    (for {
      repoName <- repoArg.toRight("please provide a repository name").right
      coordsAndMetadataString <- coordsArg.toRight("please provide coordinates").right
      jar <- jarArg.toRight("please provide a jar file").right
      jarFile <- jarArg.map(f => new jFile(f)).filter(_.exists).toRight(s"jar file: '$jar' does not exist").right
      (coords, metadata) <- Parsers.coordsMetadata(coordsAndMetadataString).right
      deps <- {
         cmdLineMsgPrintStream.println("add descriptors, enter a empty line to finish:") //TODO: move this out from this method and up where the rest of cmd line input/output is handled?
         parseInputDescriptors.right
      }
    } yield {
      val hash = whileWaiting(s"calculating hash for $coords...") { Hash.calculate(coords, jarFile) }
      val descriptor = Descriptor(coords, metadata, hash)
      whileWaiting(s"loading $coords to repository...") { 
        Repository.add(repoName, descriptor, deps)(db.database).right.map(_.toString)
      }
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
      Repository.describe(coords, meta)(db.database).right.map{ case (parent, children) => 
        (parent +: children).mkString("\n")
      }
    }).joinRight
  }
}

object InitCommand extends Command {
  override val command = "init"
  override def description = """initialises current directory with a new repository"""
  override def help = s"""
    |usage: adept $command <OPTIONAL: name>
    """.stripMargin
  override def execute(args: List[String]): Either[String, String]  = {
    val dir = Configuration.currentAdeptDir()
    if (args.size > 1)
      Left("too many repository names for init")
    else {
      val repoName = args.headOption.getOrElse(Configuration.defaultRepoName)
      Repository.init(repoName)(db.database)
    }
  }
}

object ListCommand extends Command {
  override val command = "ls"
  override def description = """list the repository"""
  override def help = s"""
    |usage: adept $command
    """.stripMargin
  override def execute(args: List[String]): Either[String, String]  = {
    val dir = Configuration.currentAdeptDir()
    if (args.size > 0)
      Left("too many args names for list")
    else {
      val repoName = args.headOption.getOrElse(Configuration.defaultRepoName)
      Repository.list(repoName)(db.database)
    }
  }
}