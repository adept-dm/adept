package adept.cli.commands

import adept.core._

object ArtifactAddCommand extends Command {
  override val command = "artifact-add"
  override def description = """todo"""
  override def help = s"""
    |usage: adept $command --server=<hostname> <repo name> <hash> <file>
    |usage: adept $command <repo name> <hash> <file> <location>
    """.stripMargin
  override def execute(args: List[String]): Either[String, String]  = {
    val dir = Configuration.currentAdeptDir()
    if (args.size > 1)
      Left("too many args names for "+command)
    else {
      val repoName = args.headOption.getOrElse(Configuration.defaultRepoName)
      Adept.diff(repoName)(db.database).right.map(modules => modules.map(m => "+"+m).mkString("\n"))
    }
  }
}