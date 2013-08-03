package adept.download

import adept.core.models._
import spray.http._
import spray.util._
import spray.can.client._
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.net.URL
import scala.util._
import java.io.File
import akka.util.duration._
import akka.actor.{Props, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import spray.http._
import spray.util._
import java.io._
import akka.dispatch.Future
import adept.utils.Logging
import akka.util.duration._
import akka.dispatch.Await
import akka.util.FiniteDuration
import java.net.URLConnection

private[adept] case class DownloadFile(url: URL, file: File)

private[adept] class DownloadActor(progressActor: Option[ActorRef]) extends Actor with Logging {
  def receive = {
    case DownloadFile(url, file) => {
      val writer = new FileOutputStream(file)
      var rd: DataInputStream= null
      val bufferSize = 2048 //semi-randomly chosen
      try {
        val conn = url.openConnection()
        //TODO: this is not right, but some parts of maven repo actually requires another user-agent so I picked this random one...
        conn.setRequestProperty("User-Agent",  "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.56 Safari/535.11")
        
        val length = conn.getContentLength()
        rd = new DataInputStream(conn.getInputStream())
        progressActor.foreach(_ ! Initialized(length))
        
        val bytes = Array.fill[Byte](bufferSize)(0)
        var bytesWrites = 0
        var bytesRead = rd.read(bytes)
        while (bytesRead != -1) {
          progressActor.foreach(_ ! Update(bytesRead))
          val writeBytes = Array.ofDim[Byte](bytesRead)
          Array.copy(bytes, 0, writeBytes, 0, bytesRead)
          writer.write(writeBytes)
          bytesWrites += bytesRead
          bytesRead = rd.read(bytes)
        }
        progressActor.foreach(_ ! Finished)
        sender ! Right(file) 
      } catch {
        case e: IOException => {
          progressActor.foreach(_ ! Failed)
          logger.error("could not download from: '" + url.toString  + "'. " + e.getCause())
          sender ! Left(e)
        }
        case e: Exception => 
          logger.error("got an unexpected error while downloading: '" + url.toString  + "'. " + e.getCause())
          sender ! Left(e)
      } finally {
        if (rd != null) rd.close()
        if (writer != null) writer.close()
      }
    }
  }
}

private[adept] object Downloader extends Logging {
  
  def download(downloadbles: Seq[(Hash, Set[String], File)], timeout: FiniteDuration, maybeProgress: Option[ActorRef] = None): Seq[Either[String, File]] = {
    if (downloadbles.nonEmpty) {
      logger.trace("downloading "+downloadbles.size+" modules...")
      
      val system = ActorSystem("adept-download")
      implicit val executionContext = akka.dispatch.ExecutionContext.defaultExecutionContext(system)
      val progressIndicator = maybeProgress match {
        case Some(progress) => progress
        case None => system.actorOf(Props[ProgressIndicator])
      }
      try {
        val perhapsFiles = (downloadbles.map{ case (hash, locations, file) =>
          val tmpFiles = locations.map{ location =>
            location -> File.createTempFile(hash.value, ".jar")
          }
          
          val possibleJars = tmpFiles.map{ case (url, file) => 
            logger.trace("temp file from "+url+" to "+file)
            progressIndicator ! Started
            Downloader.downloadOne(new URL(url), file, Some(progressIndicator))(timeout, system) //TODO: now we start download from all sources. we should have a smarter way to do this
          }
          
          Future.find(possibleJars)(_.isRight)
            .map{ maybe =>
              maybe.map(_.right.get)  -> (hash, locations, file, tmpFiles)//.get should be successful because of isRight
            }
        })
  
        
        logger.trace("waiting "+timeout+" for downloads to complete... ")
        val maybeFiles = Await.result(Future.sequence(perhapsFiles), timeout)
        maybeFiles.map{ case (maybeFile, (hash, locations, jarFile, tmpFiles)) =>
          maybeFile.map{ file =>
           val artifactHash = Hash.calculate(file)
            if (hash == artifactHash) {
              logger.trace("renaming temp file from "+file+" to "+jarFile)
              if (file.renameTo(jarFile)) {
                val deleteFiles = tmpFiles.filter{ case (_, tmpFile) => tmpFile != file }
                logger.trace("cleaning up temp files: "+{deleteFiles.mkString(",")})
                deleteFiles.foreach{ case (_, tmpFile) => tmpFile.delete() }
                Right(jarFile)
              } else {
                Left("could not rename temp file from "+file+" to "+jarFile)
              }
            } else {
              Left("expected temporary file downloaded for jar file: " + jarFile + " have hash: "+ hash+" but got "+artifactHash)
            }
          }.getOrElse{
            Left("could not download artifacts from: "+ locations.mkString(","))
          }
        }
      } finally {
        system.shutdown
      }
    } else {
      logger.trace("no modules to download...")
      Seq.empty
    }
  }
  
  def downloadOne(url: URL, file: File, maybeProgress: Option[ActorRef] = None)(implicit timeout: Timeout, system: ActorSystem): Future[Either[Exception, File]] = {
    logger.trace("client is fetching "+url+" to "+file+"..")
    
    val downloader = system.actorOf(Props(new DownloadActor(maybeProgress)))
    import akka.pattern.ask
    ask(downloader, DownloadFile(url, file)).mapTo[Either[Exception, File]]
  }
  
}