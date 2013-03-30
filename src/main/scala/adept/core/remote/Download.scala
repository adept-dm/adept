package adept.core.remote

import com.typesafe.scalalogging.slf4j.Logging
import adept.core.models._
import java.io.File
import akka.actor._
import scala.concurrent.duration._
import akka.util.Timeout
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.util._
import akka.util.Timeout

class ProgressReporter extends Actor {
  var bytesDownloaded = 0
  var totalBytes = 0
  val out = System.err
  var lastPrint = ""
  var failed = 0
  var started = 0
  var finished = 0
  var startTime: Long = -1
  
  def printProgress()  = {
    out.print("\b"*lastPrint.length)
    val mbPerSec = (bytesDownloaded.toFloat * 1000)/(1024 * (System.currentTimeMillis - startTime).toFloat )
    val completed = if (totalBytes == 0) 0f else bytesDownloaded.toFloat*100/totalBytes
    val msg = s"${finished}/${started} - %1.0f%% - %1.2f kb/s - total: %1.2f mb" format (completed, mbPerSec, totalBytes.toFloat/(1024f*1024f))
    out.print(msg)
    lastPrint = msg
  }

  def receive = {
    case Started => {
      started += 1
      if (startTime < 0) startTime = System.currentTimeMillis
      printProgress()
    }
    case Initialized(bytes) => {
      totalBytes += bytes
      printProgress()
    }
    case Failed => {
      failed += 1
      printProgress()
      if (started == (failed + finished)) {
        self ! PoisonPill
        out.println
      }
    }
    case Finished => {
      finished += 1
      printProgress()
      if (started == (failed + finished)) {
        self ! PoisonPill
        out.println
      }
    }
    case Update(bytes) => {
      bytesDownloaded += bytes
      printProgress()
    }
  }
}

trait ProgressMsgs
case object Started extends ProgressMsgs
case class Initialized(totalBytes: Int) extends ProgressMsgs
case object Failed extends ProgressMsgs
case object Finished extends ProgressMsgs
case class Update(bytes: Int) extends ProgressMsgs

private[core] object Download extends Logging {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  
  def apply(modules: Seq[(Module, File)], timeout: FiniteDuration): Seq[Try[File]] = {
    logger.trace(s"downloading ${modules.size} modules...")
    import ExecutionContext.Implicits.global
    
    val system = ActorSystem("adept-download")
    val progressReporter = system.actorOf(Props[ProgressReporter])
    try {
      val perhapsFiles = (modules.map{ case (module, file) =>
        val tmpFiles = module.artifacts.map{ artifact =>
          artifact.location -> File.createTempFile(s"${module.coords.org}-${module.coords.name}-${module.coords.version}-", ".jar")
        }
        
        val possibleJars = tmpFiles.map{ case (url, file) => 
          logger.trace(s"temp file from $url to $file")
          progressReporter ! Started
          Client.download(url, file, progressReporter)(timeout, system) //TODO: now we start download from all sources. we should have a smarter way to do this
        }
        
        Future.find(possibleJars)(_.isSuccess)
          .map{ maybeTry =>
            maybeTry.map(_.get)  -> (module, file, tmpFiles)//.get should be successful because of isSuccess
          }
      })

      
      logger.trace(s"waiting $timeout for downloads to complete... ")
      val maybeFiles = Await.result(Future.sequence(perhapsFiles), timeout)
      maybeFiles.map{ case (maybeFile, (module, jarFile, tmpFiles)) =>
        maybeFile.map{ file =>
         val artifactHash = Hash.calculate(file)
          if (module.artifactHash == artifactHash) {
            logger.trace(s"renaming temp file from $file to $jarFile")
            if (file.renameTo(jarFile)) {
              val deleteFiles = tmpFiles.filter{ case (_, tmpFile) => tmpFile != file }
              logger.trace(s"cleaning up temp files: ${deleteFiles.mkString(",")}")
              deleteFiles.foreach{ case (_, tmpFile) => tmpFile.delete() }
              Success(jarFile)
            } else {
              Failure(new Exception(s"could not rename temp file from $file to $jarFile"))
            }
          } else {
            Failure(new Exception(s"expected temporary file downloaded for module $module to have hash: ${module.artifactHash} but got $artifactHash"))
          }
        }.getOrElse{
          Failure(new Exception(s"could not download artifacts for $module from: ${module.artifacts.mkString(",")}"))
        }: Try[File]
      }
    } finally {
      system.shutdown
    }
  }
}