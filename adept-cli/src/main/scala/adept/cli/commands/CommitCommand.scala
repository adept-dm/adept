package adept.cli.commands

import adept.core.Adept
import java.io.File

object CommitCommand extends Command {
  override val command = "commit"
  override val shortDescription = "commit current modules"
  
  override def execute(args: List[String]): CommandResult ={
    val dir = Defaults.dir
    val name = Defaults.name
    
    val msgArg = args.drop(0).headOption.toRight("no commit message found")
    
    (for {
      adept <- Adept.open(dir, name).right
      msg <- msgArg.right
    } yield {
      adept.commit(msg).right.map(_.toString)
    }).joinRight.fold(l => Left(l), r => Right(Some(r)))
  }
  
}
