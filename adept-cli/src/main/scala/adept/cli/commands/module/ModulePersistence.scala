package adept.cli.commands.module

import scala.io.Source
import java.io._
import adept.core.models._
import adept.cli.commands.Defaults
import org.json4s._
import org.json4s.native.JsonMethods._

trait JsonFileSystemModulePersistance {
  private def storage = {
    val f = new File(Defaults.dir, "current-module.json")
    f.createNewFile
    f
  }

  def loadModule: Either[String, Module] = {
    try {
      val moduleJson: JValue = parse(Source.fromFile(storage).mkString)
      Module.readJsonModule(moduleJson)
    } catch {
      case e: Exception => Left(e.getMessage)
    }
  }

  def updatePersistedModule(f: Module => Module):Either[String, Module] = {
    for {
      module <- loadModule.right
    } yield {
      val updated = f(module)
      persistModule(updated)
      updated
    }
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
  def updatedPersistedModule(f: Module => Module): Either[String, Module]
  def persistModule(module: Module): Either[String, Unit]
}
