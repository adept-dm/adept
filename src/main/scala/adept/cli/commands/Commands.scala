package adept.cli.commands

import adept.core._
import java.io.{File => jFile}
import java.io.{ File => jFile }

object Commands {
  def all: Map[String, Command] = Seq(
      InitCommand,
      AddCommand,
      ListCommand,
      DescribeCommand,
      IvyAddCommand).map(c => c.command -> c).toMap
}

trait Command {
  val command: String
  def execute(args: List[String]): Either[String, String] 
  def description: String
  def help: String
}




