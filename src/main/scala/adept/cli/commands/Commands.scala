package adept.cli.commands

import adept.core._
import java.io.{File => jFile}
import java.io.{ File => jFile }
import scala.slick.session.Database
import scala.slick.session.Session

object Commands {
  def all: Map[String, Command] = Seq(
      InitCommand,
      AddCommand,
      ListCommand,
      DescribeCommand,
      CommitCommand,
      DiffCommand,
      RepoListCommand,
      IvyAddCommand).map(c => c.command -> c).toMap
}

trait Command {
  val command: String
  def checkDB(implicit database: Database): Boolean = {
    import Database.threadLocalSession
    database.withSession{
      adept.core.db.checkExistence(implicitly[Session])
    }
  }
  def execute(args: List[String]): Either[String, String] 
  def description: String
  def help: String
}




