package adept.cli.commands

import adept.core._
import java.io.File
import scala.slick.session.Database
import scala.slick.session.Session
import scala.util.Try

object Commands {
  val Version = "1.0" //TODO: get version from sbt
  
  def all: Map[String, Command] = Seq(
      SetCommand,
      DependenciesCommand,
      ServerCommand,
      CommitCommand,
      PullCommand,
      IvyCommand,
      CloneCommand,
      InitCommand,
      DownloadCommand
      ).map(c => c.command -> c).toMap
}
  
trait Command {
  import scala.language.implicitConversions
  implicit def tryToEither[A](t: Try[A]): Either[String, A] = {
    if (t.isFailure) Left(t.failed.get.getMessage)
    else Right(t.get)
  }
  
  val command: String

  def execute(args: List[String]): Either[String, String] 
  def description: String
}




