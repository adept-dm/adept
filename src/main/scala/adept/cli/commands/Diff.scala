package adept.cli.commands

import adept.core._

object DiffCommand extends Command {
  override val command = "diff"
  override def description = """todo"""
  override def help = s"""
    |usage: adept $command
    """.stripMargin
  override def execute(args: List[String]): Either[String, String]  = {
    /*
    val dir = Configuration.currentAdeptDir()
    if (args.size > 1)
      Left("too many args names for "+command)
    else {
      val repoName = args.headOption.getOrElse(Configuration.defaultRepoName)
      Adept.diff(repoName)(db.database).right.map(modules => modules.map(m => "+"+m).mkString("\n"))
    }
    */
    Left("TODO")
  }
}