package adept.core

import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import java.io.File
import org.json4s.JsonInput
import java.io.FileWriter

case class Repository(name: String, url: Option[String])
case class Config(repositories: List[Repository]) {
  def toJson = {
    import org.json4s.Extraction._
    implicit val formats = org.json4s.DefaultFormats
    import org.json4s.native.JsonMethods._
    import org.json4s.JsonDSL._
    
    decompose(this)
  }
}

object Configuration {
  
  def configFile(dir: File)= new File(dir, "config")
  
  def getRepository(dir: File, repoName: String): Option[Repository] = {
    readConfig(dir).repositories.filter(_.name == repoName).headOption
  }
  
  def updateRemote(dir: File, repoName: String, url: String) ={
    //TODO: lock files
    val oldConfig = readConfig(dir)
    val maybeRepo = oldConfig.repositories.filter(_.name == repoName).headOption
    maybeRepo.map{ oldRepo =>
      val cleanConfig = oldConfig.repositories.filter(_.name == repoName)
      val repo = oldRepo.copy(url = Some(url))
      val newConfig = oldConfig.copy(repositories = oldConfig.repositories.filter(_.name != repoName) :+ repo)
      writeConfig(dir, newConfig)
    }.getOrElse{
      throw new Exception(s"cannot update repository $repoName with url $url in ${configFile(dir)} because there is no repository with name: $repoName")
    }
  }
  
  def addRepository(dir: File, repo: Repository) ={
    //TODO: create lock file
    val file = configFile(dir)
    if (file.exists()) {
      val oldConfig = readConfig(dir)
      if (!oldConfig.repositories.contains(repo)) { 
        val newConfig = oldConfig.copy(repositories = oldConfig.repositories :+ repo)
        writeConfig(dir, newConfig)
      } else {
        throw new Exception(s"cannot add repository $repo to ${configFile(dir)} because it already exists!")
      }
    } else {
      dir.mkdirs() 
      writeConfig(dir, Config(List(repo)))
   }
  }
  
  def writeConfig(dir: File, config: Config) = {
    import org.json4s.native.JsonMethods._
    val writer = new FileWriter(configFile(dir))
    try {
      writer.write(pretty(render(config.toJson)))
    } finally {
      writer.close()
    }
  }
  
  def readConfig(dir: File): Config = {
    import org.json4s.Extraction._
    implicit val formats = org.json4s.DefaultFormats
    import org.json4s.native.JsonMethods._
    import org.json4s.JsonDSL._
    
    parse(configFile(dir)).extract[Config]
  } 
  
  val defaultArtifactDirPath = {
    "artifacts"
  }
  
  val defaultIvyConf = {
    "default"
  }
}