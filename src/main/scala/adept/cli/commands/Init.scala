package adept.cli.commands

import adept.api._

object InitCommand extends Command {
  override val command = "init"
  override def description = """initialises current directory with a new repository"""
  override def help = s"""
    |usage: adept $command <OPTIONAL: name>
    """.stripMargin
  override def execute(args: List[String]): Either[String, String]  = {
    val dir = Configuration.currentAdeptDir()
    if (args.size > 1)
      Left("too many repository names for init")
    else {
      val repoName = args.headOption.getOrElse(Configuration.defaultRepoName)
      Adept.init(repoName)(db.database)
    }
  }
}
