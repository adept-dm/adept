package adept.operations

import adept.models._
import java.io._
import org.eclipse.jgit.api.Git
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s.JArray
      
private[adept] object Add {
  def getModuleDir(dir: File, coords: Coordinates): File = { //TODO: factor out
    List(coords.org, coords.name, coords.version).foldLeft(dir){ (last,current) =>
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
    val moduleDir = getModuleDir(baseDir, module.coords)
    for {
      dir <- createDir(moduleDir).right
    } yield {
      val file = new File(dir, modulesFilename)
      val modules = if (file.exists) {
        val string = io.Source.fromFile(file).getLines.mkString("\n")
        val modules = (module +: Module.read(parse(string))).sortBy(_.artifact.hash.value)
        modules.distinct
      } else {
        List(module)
      }
      import org.json4s.Extraction._
      implicit val formats = org.json4s.DefaultFormats
      import org.json4s.native.JsonMethods._
      import org.json4s.JsonDSL._

      val json = decompose(modules)
      val writer = new FileWriter(file)
      try{
        writer.write(pretty(render(json)))
      } finally {
        writer.close()
      }
      val filepattern = file.getAbsolutePath.replace(baseDir.getAbsolutePath + File.separatorChar, "")
      git.add()
        .addFilepattern(filepattern)
        .call()
      file
    }
  }

}