package adept.core.operations.repo

import adept.core.models._
import adept.utils._
import java.io._
import org.eclipse.jgit.api.Git
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s.string2JsonInput

private[core] object Add {

  def apply(git: Git, baseDir: File, module: Module): Either[File, File] = {
    val res = for {
      file <- ModuleFiles.findAndCreate(baseDir, module.coordinates).right
    } yield {
      val modules = if (file.exists) {
        val string = io.Source.fromFile(file).getLines.mkString("\n")
        val maybeReadModules = Module.readSameCoordinates(parse(string))
        maybeReadModules.fold(
          error => throw new Exception(error),
          readModules => {
            val modules = (module +: readModules).sortBy(_.uniqueId.value)
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
