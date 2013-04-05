package adept.cli.commands

import adept.core._
import java.io.File
import adept.core.parsers.Parsers
import adept.core.models._
import adept.core.reads._

object DownloadCommand extends Command {
  override val command = "download"
  override def description = """download the jars for hashes"""

  override def execute(args: List[String]): Either[String, String] = {
    val repoName = Configuration.defaultRepoName
    val dir = Configuration.currentAdeptDir()
    if (args.length != 0) {
      val hashes = args.map(Hash.apply)
      val adept = Adept(dir, repoName)
      val timeout = Configuration.defaultTimeout
      val modules = adept.moduleFromHashes(hashes.toSet).toSeq
      val nonFound = hashes.toSeq.map(_.value).diff(modules.map(_.hash.value))
      if (nonFound.isEmpty) {
        val perhapsFiles = adept.download(modules.map(m => m -> None))(timeout)
        perhapsFiles.map { files =>
          files.map(_.getAbsolutePath).mkString("\n")
        }: Either[String, String]
      } else {
        Left(s"could not find modules for the following hashes: ${nonFound.mkString(",")}")
      }
    } else {
      Left("could not find the correct arguments and options")
    }
  }
}