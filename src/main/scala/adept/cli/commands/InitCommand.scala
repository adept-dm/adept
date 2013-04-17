package adept.cli.commands

import adept.Adept
import java.io.File

object InitCommand extends Command {
  override val command = "init"
  override val shortDescription = "initialize adept here"
  
  override def execute(args: List[String]): Either[String, String] ={
    val dir = Defaults.dir
    val name = Defaults.name
    Adept.init(dir, name).right.map{ _ =>
      "initialized "+name+" in "+dir
    }
  }
  
}