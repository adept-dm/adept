package adept.cli.commands

import adept.api._

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
      Adept.list(repoName)(db.database)
    }
  }
}