package adept.cli.commands

import java.io.File
import adept.cli.commands._

object Commands {
  val Version = "1.0" //TODO: get version from sbt
  
  def all: Map[String, Command] = Seq[Command](
      InitCommand,
      IvyAddCommand,
      CommitCommand,
      CloneCommand
      ).map(c => c.command -> c).toMap
}
  
trait Command {
  val command: String

  def execute(args: List[String]): Either[String, String] 
  def shortDescription: String
}




