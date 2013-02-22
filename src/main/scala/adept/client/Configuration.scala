package adept.client

import java.io.{File => jFile} 

object Configuration {

  def workingDir = new jFile(".").getCanonicalPath()
  
  def currentAdeptDir(dir: String = workingDir) = new jFile(dir, ".adept")  

}