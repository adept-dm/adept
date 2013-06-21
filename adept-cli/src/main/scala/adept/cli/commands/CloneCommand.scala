package adept.cli.commands

import adept.core.Adept
import java.io.File
import adept.core.models.Coordinates
import adept.ivy.IvyHelpers

object CloneCommand extends Command {
  override val command = "clone"
  override val shortDescription = "clone a repository"
  
  override def execute(args: List[String]): CommandResult ={
    val dir = Defaults.dir
    val name = Defaults.name
    
    val uriArg = args.drop(0).headOption.toRight("cold not find an uri to clone from")
    
    (for{
      uri <- uriArg.right
    } yield{
      Adept.clone(dir, name, uri).right.map{ _ =>
        name+ " was cloned successfully"
      }
    }).joinRight.fold(l => Left(l), r => Right(Some(r)) )
  }
  
}
