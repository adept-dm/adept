package adept.utils

import adept.models._

import java.io.File

object ModuleFiles {

  def getModuleDir(baseDir: File, coords: Coordinates): File = {
    List(coords.org, coords.name, coords.version).foldLeft(baseDir) { (last, current) =>
      new File(last, current)
    }
  }

  def findAndCreate(baseDir: File, coords: Coordinates): Either[File, File] = {
    val dir = getModuleDir(baseDir, coords)
    if (dir.isDirectory || (!dir.isDirectory && dir.mkdirs)) {
      val file = new File(dir, modulesFilename)
      Right(file)
    } else {
      Left(dir)
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

}
