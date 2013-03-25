package adept.core

import java.io.File
import collection.parallel.immutable.ParSeq
import adept.core.models.{Hash, Module}
import adept.core.operations._
import adept.core.remote._
import util._
import adept.core.db.Checkpoints
import scala.concurrent.duration.Duration

object Adept {
  
  def apply(dir: File, repoName: String): Adept = {
    new Adept(dir, repoName)
  }
  
  def init(dir: File, repoName: String): Try[Adept] = {
    if (dir.exists)
     Failure(new Exception(s"cannot init adept in $dir because it exists already"))
    else 
     Success((new Adept(dir, repoName)).init())
  }
  
  import adept.core.db.DAO.driver.simple._
  
  private def database(prefixFile: File) = {
    //21 == 2**21 bytes == 2 Mb
    Database.forURL("jdbc:h2:split:21:"+ prefixFile.toURI, driver = "org.h2.Driver") 
  } 
  
  private def openMainDB(workingDir: File) = database(new File(workingDir, "main"))
  private def openStagedDB(workingDir: File) = database(new File(workingDir, "staged"))
    
  def clone(destDir: File, srcDir: File, repoName: String): Try[Hash] = {
    val adept = new Adept(destDir, repoName)
    val workingDir = adept.workingDir
    val fromDir = new File(srcDir, repoName)
    if (workingDir.exists) {
      Failure(new Exception(s"Cannot clone $repoName to $destDir because $workingDir exists already"))
    } else if (!fromDir.exists || !fromDir.isDirectory) {
      Failure(new Exception("Could not find expected directory: " + fromDir.getAbsolutePath))
    } else {
      workingDir.mkdirs()
      Option(fromDir.listFiles).map{ files => //TODO: move to operation (or remote)
        files.foreach{ src =>
          import java.io._
          val dest = new File(workingDir, src.getName())
          val fos = new FileOutputStream(dest)
          val fis = new FileInputStream(src)
          try {
            fos.getChannel() transferFrom(
                fis.getChannel, 0, Long.MaxValue)
          } finally {
             if (fos != null) fos.close()
             if (fis != null) fis.close()
          }
        }
      }
      
      val newStaged = openStagedDB(workingDir)
      val newMain = openMainDB(workingDir)
      newMain.withTransaction{ mainSession: Session =>
        Common.onlyOption(Queries.lastCommit)(mainSession).map{ lastCommit =>
          newStaged.withSession{ implicit session: Session =>
            Checkpoints.autoInc.insert(lastCommit.hash.value)
          }
          Success(lastCommit.hash)
        }.getOrElse{
          Failure(new Exception("Could not find a commit to clone from in " + fromDir.getAbsolutePath))
        }
      }
    }
    
  }
  
  
}

class Adept protected(val dir: File, repoName: String) {
  import adept.core.db.DAO.driver.simple._
  private[core] val workingDir = new File(dir, repoName)
  
  
  protected[core] lazy val mainDB = Adept.openMainDB(workingDir)
  protected[core] lazy val stagedDB = Adept.openStagedDB(workingDir)
  
  protected def init(): Adept = {
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
  
  def pull(remoteRepoName: String, host: String, port: Int, timeout: Duration): Try[Hash] = {
    Pull(mainDB, stagedDB, remoteRepoName, host, port, timeout)
  }
  
  def server(repoName: String) = {
    val adeptServer = Server(mainDB, repoName)
    adeptServer.start(1337)
  }
}
