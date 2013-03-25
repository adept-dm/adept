package adept.cli.commands

import java.io.File
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

object Configuration {

  val defaultTimeout = Duration(5, TimeUnit.MINUTES)
  val pullTimeout = defaultTimeout
  
  
  val defaultRepoName = "local"
  
  def workingDir = new File(".").getCanonicalPath()
  
  val adeptDir = ".adept"
    
  def currentAdeptDir(dir: String = workingDir) = new File(dir, adeptDir)  

}