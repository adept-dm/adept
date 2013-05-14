package adept.tools

import java.io._
import scala.Array.canBuildFrom

object AdeptIvyCacheReader {

  def listFiles(dir: File)(block : File => Boolean) = {
    dir.listFiles(new FileFilter{
      override def accept(file: File): Boolean = {
        block(file)
      }
    })
  }
  
  
  
  def main(args: Array[String]): Unit = {
    val ivyHome = new File(Option(System.getProperty("ivy.home")).getOrElse{
      System.getProperty("user.home") + File.separator + ".ivy2"
    })

    val outputFile = args.headOption.map(new File(_)).getOrElse{
      val msg = "Need output filename as an argument!"
      val help = """Synopsis: scans ivy cache directory for modules and creates a file with coordinates""".stripMargin
      System.err.println(msg + "\n" + help)
      throw new Exception(msg)
    }
    
    val cacheDir = new File(ivyHome, "cache")
    
    println("searching for directories in: " + cacheDir)
    val level1Dirs = listFiles(cacheDir)( file => file.exists && file.isDirectory)
    
    
    println("scanning for nested dirs in " + level1Dirs.size + " dirs...")
    val level2Dirs = level1Dirs.flatMap{ dir => 
      listFiles(dir)( file => file.exists && file.isDirectory )
    }
    
    println("scanning for ivy xmls in " + level2Dirs.size + " dirs...")
    val ivyXmls = level2Dirs.flatMap{ dir =>
      listFiles(dir)( file => file.getName.startsWith("ivy") && file.getName.endsWith(".xml") )
    }
    
    val total = ivyXmls.size
    
    var current = 0
    var lastMsg = ""
    
    println("extracting coordinates from " + total + " files...")
    val coords = ivyXmls.par.foldLeft(Set.empty[String]){ (all, f) => //cheap trick to be a bit parallel...
      val coords = for {
        modules <- xml.XML.loadFile(f) \\ "ivy-module"
        info <- modules \ "info"
        organisation <- info \ "@organisation"
        revision <- info \ "@revision"
        module <- info \ "@module"
      } yield {
        organisation + ":" + module + ":" + revision
      }
      current += 1
      print("\r" * lastMsg.size)
      lastMsg = "completed: %1.0f%%" format (current.toFloat*100/total.toFloat) 
      print(lastMsg)
      all ++ coords.toSet
    }
    println()
    println("dumping " + coords.size + " coordinates to " + outputFile.getAbsolutePath + "...")
    val writer = new FileWriter(outputFile)
    try {
      writer.write(coords.mkString("\n"))
    } finally {
      writer.close()
    }
    println("done!")
  }
  
}
