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
import akka.actor.ActorSystem

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
    if (dir.exists && Configuration.configFile(dir).exists) {
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
  
  private[core] def clonedInit(dir: File, repoName: String, url: Option[String]): Adept = {
    (new Adept(dir, Repository(repoName, url))).clonedInit()
  }
  
  def init(dir: File, repoName: String, url: Option[String]): Try[Adept] = {
    if (exists(dir, repoName))
     Failure(new Exception(s"cannot init adept $repoName in $dir because it exists already"))
    else 
     Success((new Adept(dir, Repository(repoName, url))).init())
  }
  
  private def database(prefixFile: File) = {
    //21 == 2**21 bytes == 2 Mb
    Database.forURL("jdbc:h2:split:21:"+ prefixFile.toURI, driver = "org.h2.Driver") 
  } 
  
  val mainDBName = "main"
  val stagedDBName = "staged"
  private[core] def openMainDB(workingDir: File) = {
    database(new File(workingDir, mainDBName))
  }
  private[core] def openStagedDB(workingDir: File) = database(new File(workingDir, stagedDBName))
  private[core] def checkpointFile(workingDir: File): File = new File(workingDir, "CHECKPOINT") 
  private[core] def workingDir(dir: File, repoName: String) = new File(dir, repoName)
  
  def clone(baseDir: File, src: String, repoName: String): Try[Hash] = {
    Clone(baseDir, src, repoName, workingDir(baseDir, repoName))  
  }
}

class Adept protected(val dir: File, val repo: Repository) extends Logging {
  import adept.core.db.DAO.driver.simple._
  private[core] val workingDir = Adept.workingDir(dir, repo.name)
  
  protected[core] lazy val mainDB = Adept.openMainDB(workingDir)
  protected[core] lazy val stagedDB = Adept.openStagedDB(workingDir)
  protected[core] lazy val checkpointFile = Adept.checkpointFile(workingDir)
  
  private[core] def clonedInit() = {
    Configuration.addRepository(dir, repo)
    stagedDB.withSession{s: Session => adept.core.db.DAO.stagedDDLs.create(s) }
    this
  }
  
  protected def init(): Adept = {
    clonedInit()
    mainDB.withSession{s: Session => adept.core.db.DAO.mainDDLs.create(s) }
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
  
  def dump: List[(Module, Option[Hash], Boolean)] = {
    Dump(mainDB)
  }
  
  def server = {
    val adeptServer = Server(mainDB, repo.name)
    adeptServer.start(1337)
  }
}
