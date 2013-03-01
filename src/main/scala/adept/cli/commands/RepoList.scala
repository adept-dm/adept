package adept.cli.commands

import adept.core._

object RepoListCommand extends Command {
  override val command = "repo-list"
  override def description = """todo"""
  override def help = s"""
    |usage: adept $command
    """.stripMargin
  override def execute(args: List[String]): Either[String, String]  = {
    val dir = Configuration.currentAdeptDir()
    if (args.size != 0)
      Left("too many args names for "+command)
    else {
      Right(Adept.repoList(db.database).map{ case (r, stagedVersion) =>
        r.toString + stagedVersion.map( v => s" (staged: $v)").getOrElse("")
      }.mkString("\n"))
    }
  }
}