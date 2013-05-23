package adept.cli.commands.module

import java.io._
import adept.models._
import org.json4s._
import org.json4s.native.JsonMethods._

trait JsonFileSystemModulePersistance {
  private def storage = {
    val f = new File("./blah.txt")
    f.createNewFile
    f
  }

  def persistModule(module: Module) = {
    val json = Module.writeJsonModule(module)
    val str = pretty(render(json))
    val writer = new FileWriter(storage)
    writer.write(str.toString)
    writer.close
    Right(())
  }

}

trait ModulePersistence {
  def persistModule(module: Module): Either[String, Unit]
}
