package adept.cli.commands

import java.io.{File => jFile} 
import java.io.{File => jFile}

object Configuration {

  
  val defaultRepoName = "local"
  
  def workingDir = new jFile(".").getCanonicalPath()
  
  def currentAdeptDir(dir: String = workingDir) = new jFile(dir, ".adept")  

}