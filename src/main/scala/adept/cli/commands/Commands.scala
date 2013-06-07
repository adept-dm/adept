package adept.cli.commands

import java.io.File
import adept.cli.CliHelpers
import adept.cli.commands._
import module._

object Commands {
  val Version = "1.0" //TODO: get version from sbt

  def all: Map[String, Command] = CliHelpers.all(
    InitCommand,
    IvyAddCommand,
    CommitCommand,
    CloneCommand,
    ModuleCommand
  )
}

trait Command {
  val command: String
  type CommandResult = Either[String, Option[String]]
  /**
    * @return Left(error message) or Right(optional message here)
    */
  def execute(args: List[String]): CommandResult
  def executeWithHelpCheck(args: List[String]) = {
    if (args == List("-h") || args == List("--help")) {
      Right(help)
    } else {
      execute(args)
    }
  }
  def shortDescription: String
  def help: Option[String] = None
}
