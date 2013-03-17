package adept.cli.commands

import adept.core._
import java.io.{File => jFile}
import java.io.{ File => jFile }
import scala.slick.session.Database
import scala.slick.session.Session

object Commands {
  val Version = "1.0"
  
  def all: Map[String, Command] = Seq(
      SetCommand,
      ServerCommand,
      InitCommand
      ).map(c => c.command -> c).toMap
}

trait Command {
  val command: String

  def execute(args: List[String]): Either[String, String] 
  def description: String
  def help: String
}




