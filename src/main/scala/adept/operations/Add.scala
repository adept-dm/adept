package adept.operations

import adept.models._
import java.io._
import org.eclipse.jgit.api.Git
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s.JArray

private[adept] object Add {
  def getModuleDir(dir: File, coords: Coordinates): File = { //TODO: factor out
    List(coords.org, coords.name, coords.version).foldLeft(dir) { (last, current) =>
      new File(last, current)
    }
  }

  def createDir(dir: File): Either[File, File] = {
    if (dir.exists && dir.isDirectory) {
      Right(dir)
    } else {
      if (dir.mkdirs) {
        Right(dir)
      } else {
        Left(dir)
      }
    }
  }
  val modulesFilename = "modules.json"

  def apply(baseDir: File, module: Module): Either[File, File] = {
    val git = Git.open(baseDir)
    val moduleDir = getModuleDir(baseDir, module.coordinates)
    val res = for {
      dir <- createDir(moduleDir).right
    } yield {
      val file = new File(dir, modulesFilename)
      val modules = if (file.exists) {
        val string = io.Source.fromFile(file).getLines.mkString("\n")
        val maybeReadModules = Module.readSameCoordinates(parse(string))
        maybeReadModules.fold(
          error => throw new Exception(error),
          readModules => {
            val modules = (module +: readModules).sortBy(_.artifacts.hashCode)
            modules.distinct
          })
      } else {
        List(module)
      }
      val grouped = modules.groupBy(_.coordinates)
      if (grouped.size > 1) throw new Exception("did not expect to have more than one coords: " + modules) 
      grouped.headOption.map{ case (coords, modules) =>
        val json = Module.writeJsonForSameCoords(coords, modules)
        val writer = new FileWriter(file)
        try {
          writer.write(pretty(render(json)))
        } finally {
          writer.close()
        }
        val filepattern = file.getAbsolutePath.replace(baseDir.getAbsolutePath + File.separatorChar, "")
        git.add()
          .addFilepattern(filepattern)
          .call()
        Right(file)
      }.getOrElse{
        Left(file) 
      }
    }
    res.joinRight
  }

}