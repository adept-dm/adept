package adept.utils

import adept.core.models._

import java.io.File

object ModuleFiles {

  private val modulesFilename = "modules.json"

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
  
  def getModuleFile(baseDir: File, coords: Coordinates): File = {
    new File(getModuleDir(baseDir, coords), modulesFilename)
  }

}
