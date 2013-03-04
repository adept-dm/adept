package adept.cli.commands

import adept.core._
import adept.core.Configuration;
import java.io.{ File => jFile }

object AddCommand extends Command {
  override val command = "add"
  override def description = """adds a dependency to current repository"""
  override def help = s"""
    |usage: adept $command --server=<hostname> <repository name> <coordinates> <jar file>
    |       adept $command <repository name> <coordinates> <jar file> <jar url>
    |       adept $command <repository name> <coordinates> <hash>
    """.stripMargin
  override def execute(args: List[String]): Either[String, String] = {
    
    def randomString = {
      import java.security.SecureRandom
      import java.math.BigInteger


      val random = new SecureRandom();

      new BigInteger(130, random).toString(14);
    }

    
    (1 to 50000).foreach { i =>
      val m = Module(Coordinates(randomString + i, randomString+ i,randomString+ i), Metadata(Map("test" -> "yes")), Hash(randomString+ i), Set(Artifact(randomString+ i)), Hash(randomString+ i))
      println(Adept.add("local", m)(db.database))
    }
      println(Adept.commit("local")(db.database))
      import db.driver.simple._
      db.database.withSession{
        import Database.threadLocalSession
        Right("current count: " + Query(Query(Modules).length).firstOption)
    }
    /*
    val repoDir = Configuration.currentAdeptDir()
    
    val repoArg = args.drop(0).take(1).headOption
    val coordsArg = args.drop(1).take(1).headOption
    val jarArg = args.drop(2).take(1).headOption
    
    def parseInputModules: Either[String, List[Module]] = {
      val reader = {
        import java.io._
        new BufferedReader(new InputStreamReader(System.in));
      }
      @annotation.tailrec def slurpInput(lines: List[String]): List[String] = {
        val line = reader.readLine()
        if (line != null && line.nonEmpty) slurpInput(line :: lines)
        else lines
      }
      slurpInput(List.empty).foldLeft(Right(List.empty): Either[String, List[Module]]){ 
        (current, string) => {
          for {
            currentModules <- current.right
            module <- Parsers.module(string).right
          } yield {
            module +: currentModules 
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
      coordsAndMetadataString <- coordsArg.toRight("please provide coordinates (basic: <org>:<name>:<version>, with metadata: <org>:<name>:<version>[key=value, ...])").right
      jar <- jarArg.toRight("please provide a jar file").right
      jarFile <- jarArg.map(f => new jFile(f)).filter(_.exists).toRight(s"jar file: '$jar' does not exist").right
      (coords, metadata) <- Parsers.coordsMetadata(coordsAndMetadataString).right
      deps <- {
         cmdLineMsgPrintStream.println("add dependencies (format: <org>:<name>:<version>[key=value,...]@<hash>), enter a empty line to finish:") //TODO: move this out from this method and up where the rest of cmd line input/output is handled?
         parseInputModules.right
      }
    } yield {
      val hash = whileWaiting(s"calculating hash for $coords...") { Hash.calculate(coords, jarFile) }
      val module = Module(coords, metadata, hash, deps.map(_.hash)) //TODO: no need to get moduels
      whileWaiting(s"loading $coords to repository...") { 
        Adept.add(repoName, module)(db.database).right.map(_.toString)
      }
    }).joinRight*/
    
  }
}