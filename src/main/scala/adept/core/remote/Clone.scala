package adept.core.remote

import adept.core.Adept
import java.io._
import scala.util._
import adept.core.operations._
import adept.core.models._
import com.typesafe.scalalogging.slf4j.Logging
import akka.actor._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import java.util.zip.ZipFile
import java.util.zip.ZipInputStream
import scala.concurrent.Await
import org.h2.tools.Recover
import org.h2.tools.Restore
import java.net.URL

private[core] object Clone extends Logging {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._

   def tmpDir(prefix: String, suffix: String): Option[File] = { //TODO: move to FileUtils? and replace junit temporary dirs in test
      val baseDir = new File(System.getProperty("java.io.tmpdir"))
      val baseName = prefix + System.currentTimeMillis() + "-" + suffix;
      val attempts = 1000
      var dir: Option[File] = None
      var counter = 0
      while (dir == None && counter < attempts) {
        val current = new File(baseDir, baseName)
        counter += 1
        if (current.mkdir()) {
          dir = Some(current)
        }
      }
      dir
    }
  
  def recursiveDelete(file: File) : Unit = {
    def deleteFile(dirOrFile : File) : Unit = { //TODO: tailrec or use some lib
      if(dirOrFile.isDirectory)
        dirOrFile.listFiles.foreach{ f => deleteFile(f) }
      dirOrFile.delete()
    }
    deleteFile(file)
  }
    
  def apply(baseDir: File, src: String, repoName: String, workingDir: File): Try[Hash] = {
    logger.trace(s"cloning to $workingDir with $repoName from $src (adept dir: $baseDir)...")
    val exists = Adept.exists(baseDir, repoName)
    val currentDir = tmpDir("adept-", "clone").getOrElse{
      throw new Exception("could not create a temporary directory to clone to...")
    }
    var url: Option[String] = None//TODO: FIX THIS VAR!
    
    val directory = if (exists) {
      Failure(new Exception(s"cannot clone $repoName to $baseDir because it exists already"))
    } else if (src.startsWith("http")) {
      import concurrent.duration._
      val system = ActorSystem("adept-clone")
      try {
        url = Some(src)
        Clone.remote(src, currentDir, 5 minutes span)(system)
      } finally  {
        system.shutdown
      }
    } else {
      val fromDir = new File(src)
      if (fromDir.exists && fromDir.isDirectory) {
        Clone.local(fromDir, currentDir)
      } else {
        Failure(new Exception(s"cannot clone from $fromDir because it is not a directory"))
      }
    }
    directory.failed.foreach(_ => recursiveDelete(currentDir) )
    
    val checkpoint = directory.flatMap{ _ => //writing checkpoint...
      Adept.openMainDB(currentDir).withTransaction{ mainSession: Session =>
        Common.onlyOption(Queries.lastCommit)(mainSession).map{ lastCommit =>
          if (Adept.exists(baseDir, repoName)) {
            Failure(new Exception(s"an adept directory was created in $baseDir for $repoName while cloning!"))
          } else {
            Success(lastCommit.hash)
          }
        }.getOrElse{
          Failure(new Exception("Could not find a commit to clone from: " + src))
        }
      }
    }
    
    checkpoint.foreach{ lastCommit =>
      workingDir.mkdirs()
      Checkpoint.write(Adept.checkpointFile(workingDir), lastCommit)
      currentDir.listFiles().foreach{ f =>
        val dest = new File(workingDir, f.getName)
        logger.trace(s"renaming $f to $dest")
        f.renameTo(dest)
      }
      Adept.clonedInit(baseDir, repoName, url)
    }
    checkpoint
  }
  
  def remote(url: String, workingDir: File, timeout: FiniteDuration)(system: ActorSystem):Try[File] = {
    logger.trace(s"cloning to $workingDir...")
    import concurrent.ExecutionContext.Implicits.global
    val tmpFile = File.createTempFile(workingDir.getName, ".zip")
    try {
      val progressIndicator = system.actorOf(Props[ProgressIndicator])
      progressIndicator ! Started
      val downloadURL = new URL(url + "/clone")
      val perhapsZipFile = Client.download(downloadURL,tmpFile, progressIndicator)(timeout, system)
      val perhapsDBFiles = perhapsZipFile.map{ tryZipFile =>
        tryZipFile.map { zipFile =>
          val dbName = null //db - the database name (null for all databases)
          logger.trace(s"restoring from: "+ zipFile + " to " + workingDir)
          Restore.execute(zipFile.getAbsolutePath, workingDir.getAbsolutePath, dbName) 
        }
      }
      logger.trace(s"waiting for restoring to complete for $timeout...")
      val result = Await.result(perhapsDBFiles, timeout)
      result.map(_ => workingDir )
    } finally {
      tmpFile.delete()
    }
  }
  
  def local(fromDir: File, workingDir: File): Try[File] = {
    //TODO: this won't work if there is stuff happening in the DB, use BACKUP to zip for this instead
    workingDir.mkdirs()
    val output = Option(fromDir.listFiles).toList.flatMap{ files =>
      files.map{ src =>
        import java.io._
        val dest = new File(workingDir, src.getName())
        val fos = new FileOutputStream(dest)
        val fis = new FileInputStream(src)
        try {
          fos.getChannel() transferFrom(
              fis.getChannel, 0, Long.MaxValue) //FIXME: Long.MaxValue will break us in the future...
        } finally {
           if (fos != null) fos.close()
           if (fis != null) fis.close()
        }
        dest
      }
    }
    Success(workingDir)
  }
}