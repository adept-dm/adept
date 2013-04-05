package adept.core

import java.io.File
import collection.parallel.immutable.ParSeq
import adept.core.models.{Hash, Module, Coordinates}
import adept.core.operations._
import adept.core.remote._
import util._
import scala.concurrent.duration.FiniteDuration
import adept.core.reads._
import adept.core.models.Checkpoint
import java.net.URL
import com.typesafe.scalalogging.slf4j.Logging

object Adept {
  import adept.core.db.DAO.driver.simple._

  def apply(dir: File, repoName: String): Adept = {
    Configuration.getRepository(dir, repoName).map{ repo =>
      new Adept(dir, repo)
    }.getOrElse{
      throw new Exception(s"cannot find Adept with name $repoName in $dir")
    }
  }
  
  def repositories(dir: File): Seq[Adept] = {
    if (Configuration.configFile(dir).exists) {
      Configuration.readConfig(dir).repositories.map{ repo =>
        Adept(dir, repo.name)
      }
    } else Seq.empty
  }
  
  def exists(dir: File): Boolean = {
    Configuration.configFile(dir).exists
  }
  
  def exists(dir: File, repoName: String): Boolean = {
    if (dir.exists) {
      val a = Adept(dir, repoName)
      val workingDir = a.workingDir
      
      openMainDB(workingDir).withSession{ implicit session: Session =>
        import adept.core.db._
        import scala.slick.jdbc.{StaticQuery => Q}
        val tables = Q.queryNA[String](s"""SELECT table_name FROM information_schema.tables""").list
        
        DAO.mainTables.map(_.tableName).diff(tables).isEmpty //all tables are accounted for
      }
    } else false
  }
  
  def init(dir: File, repoName: String, url: Option[String]): Try[Adept] = {
    if (dir.exists)
     Failure(new Exception(s"cannot init adept in $dir because it exists already"))
    else 
     Success((new Adept(dir, Repository(repoName, url))).init())
  }
  
  private def database(prefixFile: File) = {
    //21 == 2**21 bytes == 2 Mb
    Database.forURL("jdbc:h2:split:21:"+ prefixFile.toURI, driver = "org.h2.Driver") 
  } 
  
  val mainDBName = "main"
  val stagedDBName = "staged"
  private def openMainDB(workingDir: File) = database(new File(workingDir, mainDBName))
  private def openStagedDB(workingDir: File) = database(new File(workingDir, stagedDBName))
  private def checkpointFile(workingDir: File): File = new File(workingDir, "CHECKPOINT") 
  
  def clone(destDir: File, src: String, repoName: String): Try[Hash] = {
    val exists = Adept.exists(destDir, repoName)
    val adept = if (exists) {
      Failure(new Exception(s"cannot clone $repoName to $destDir because it exists already"))
    } else if (src.startsWith("http")) {
      val url = src
      import concurrent.duration._
      Adept.init(destDir, repoName, Some(src)).flatMap{ adept =>
        Clone.remote(src, adept.workingDir, 5 minutes span).map{ _ =>
          adept
        }
      }
    } else if (destDir.exists && !destDir.isDirectory) {
      Failure(new Exception(s"cannot clone from $destDir because it is not a directory"))
    } else {
      Adept.init(destDir, repoName, Some(src)).flatMap{ adept =>
        val fromDir = new File(src)
        Clone.local(fromDir, adept.workingDir).map{ _ =>
          adept
        }
      }
    }
    
    val checkpoint = adept.flatMap{ adept => //writing checkpoint...
      openMainDB(adept.workingDir).withTransaction{ mainSession: Session =>
        Common.onlyOption(Queries.lastCommit)(mainSession).map{ lastCommit =>
          Checkpoint.write(checkpointFile(adept.workingDir), lastCommit.hash)
          Success(lastCommit.hash)
        }.getOrElse{
          Failure(new Exception("Could not find a commit to clone from: " + src))
        }
      }
    }
    /*TODO: cleanup and use a temp file which is moved into place
     checkpoint.failed.foreach{ _ =>
      destDir.delete
    }*/
    checkpoint
  }
  
  
}

class Adept protected(val dir: File, val repo: Repository) extends Logging {
  import adept.core.db.DAO.driver.simple._
  private[core] val workingDir = new File(dir, repo.name)
  
  
  protected[core] lazy val mainDB = Adept.openMainDB(workingDir)
  protected[core] lazy val stagedDB = Adept.openStagedDB(workingDir)
  protected[core] lazy val checkpointFile = Adept.checkpointFile(workingDir)
  
  protected def init(): Adept = {
    Configuration.addRepository(dir, repo)
    mainDB.withSession{s: Session => adept.core.db.DAO.mainDDLs.create(s) }
    stagedDB.withSession{s: Session => adept.core.db.DAO.stagedDDLs.create(s) }
    this
  }
  
  def commit(): Try[Hash] = {
    Commit(stagedDB, mainDB)
  }
  
  def delete(hash: Hash): Try[Hash] = {
    Delete(hash, stagedDB, mainDB)
  }
  
  def set(module: Module): Try[Hash] = {
    Set(module, stagedDB, mainDB)
  }
  
  def dependencies(coords: Coordinates): Try[Set[(Module, Hash)]] = {
    Dependencies(coords, None, mainDB, stagedDB)
  }
  
  def lastCommit(hashes: Set[Hash]): Option[Hash] = {
    LastCommit(hashes, mainDB)
  }
  
  def pull(hash: Hash)(implicit timeout: FiniteDuration): Try[Hash] = {
    repo.url.map{ url =>
      Pull(repo.name, new URL(url), hash, timeout, checkpointFile, mainDB, stagedDB)
    }.getOrElse{
      Failure(new Exception(s"cannot pull from repository ${repo.name} because it has no remote urls specified"))
    }
  }
  
  def pull(implicit timeout: FiniteDuration): Try[Hash] = {
    repo.url.map{ url =>
      Pull(repo.name, new URL(url), timeout, checkpointFile, mainDB, stagedDB)
    }.getOrElse{
      Failure(new Exception(s"cannot pull from repository ${repo.name} because it has no remote urls specified"))
    }
  }
  
  def moduleFromHashes(hashes: Set[Hash]): Set[Module] = {
    GetModules(hashes, mainDB)
  }
  
  def download(modulesDest: Seq[(Module, Option[File])])(implicit timeout: FiniteDuration): Try[Seq[File]] ={
    
    val modulesFiles = modulesDest.map{ case (module, dest) =>
      module -> dest.getOrElse{
        val artifactDir = new File(dir, Configuration.defaultArtifactDirPath)
        artifactDir.mkdirs
        new File(artifactDir, module.artifactHash.value+".jar") //TODO: need a smarter way to store artifacts (imagine 50K jars in one dir!)
      }
    }
    logger.trace("locating existing and non existing files...")
    val (existing, nonExisting) = modulesFiles.partition{ case (module, file) => file.exists && Hash.calculate(file) == module.artifactHash}
    logger.trace(s"found ${existing.length} existing and ${nonExisting.length} non existing...")
    for {
      existingFiles <- TryHelpers.reduce(existing.map{ case (_, file ) => Success(file) })
      downloadedFiles <- TryHelpers.reduce(Download(nonExisting, timeout))
    } yield {
      existingFiles ++ downloadedFiles
    }
  }
  
  def server = {
    val adeptServer = Server(mainDB, repo.name)
    adeptServer.start(1337)
  }
}
