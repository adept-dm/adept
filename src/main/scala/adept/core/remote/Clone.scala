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

  def remote(url: String, workingDir: File, timeout: FiniteDuration):Try[Unit] = {
    logger.trace(s"cloning to $workingDir...")
    import concurrent.ExecutionContext.Implicits.global
    val system = ActorSystem("adept-clone")
    val progressIndicator = system.actorOf(Props[ProgressIndicator])
    progressIndicator ! Started
    val tmpFile = File.createTempFile(workingDir.getName, ".zip")
    tmpFile.deleteOnExit()
    val downloadURL = new URL(url + "/clone")
    val perhapsZipFile = Client.download(downloadURL,tmpFile, progressIndicator)(timeout, system)
    val perhapsDBFiles = perhapsZipFile.map{ tryZipFile =>
      tryZipFile.map { zipFile =>
        val dbName = null //db - the database name (null for all databases)
        Restore.execute(zipFile.getAbsolutePath, workingDir.getAbsolutePath, dbName) 
      }
    }
    logger.trace(s"waiting for dowloads to complete for $timeout...")
    val result = Await.result(perhapsDBFiles, timeout)
    system.shutdown
    result.map(_ => () )
  }
  
  def local(fromDir: File, workingDir: File): Try[Unit] = {
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
              fis.getChannel, 0, Long.MaxValue)
        } finally {
           if (fos != null) fos.close()
           if (fis != null) fis.close()
        }
        dest
      }
    }
    Success()
  }
}