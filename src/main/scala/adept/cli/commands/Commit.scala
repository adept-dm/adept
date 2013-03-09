package adept.cli.commands

import adept.core._

object CommitCommand extends Command {
  override val command = "commit"
  override def description = """todo"""
  override def help = s"""
    |usage: adept $command <OPTIONAL: repo>
    """.stripMargin
  override def execute(args: List[String]): Either[String, String]  = {
    /*
    val dir = Configuration.currentAdeptDir()
    if (args.size > 1)
      Left("too many args names for "+command)
    else {
      val repoName = args.headOption.getOrElse(Configuration.defaultRepoName)
      Adept.commit(repoName)(db.database).right.map(_.toString)
    }*/
    Left("TODO")
  }
}