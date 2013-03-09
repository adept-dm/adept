package adept.cli.commands

import adept.core._

object ListCommand extends Command {
  override val command = "ls"
  override def description = """list the repository"""
  override def help = s"""
    |usage: adept $command
    """.stripMargin
  override def execute(args: List[String]): Either[String, String] = {
    /*
    val dir = Configuration.currentAdeptDir()
    if (args.size > 0)
      Left("too many args names for "+command)
    else {
      val repoName = args.headOption.getOrElse(Configuration.defaultRepoName)
      Adept.list(repoName)(db.database).right.map(_.map{ 
        case (module, repo) =>
          module + "!"+repo
      }.mkString("\n"))
      
    }
    */
    Left("TODO")
  }
}