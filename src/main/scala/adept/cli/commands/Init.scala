package adept.cli.commands

import adept.core._
import scala.slick.session.Database

object InitCommand extends Command {
  override val command = "init"
  override def description = """initialises current directory with a new repository"""
  override def help = s"""
    |usage: adept $command <OPTIONAL: name>
    """.stripMargin
  
  override def checkDB(implicit db: Database): Boolean = {
    true //no need for init
  }  
  
  override def execute(args: List[String]): Either[String, String]  = {
    val dir = Configuration.currentAdeptDir()
    if (args.size > 1)
      Left("too many repository names for "+command)
    else {
      val repoName = args.headOption.getOrElse(Configuration.defaultRepoName)
      Adept.init(repoName)(db.database)
    }
  }
}
