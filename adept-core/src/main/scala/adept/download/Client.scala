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

case class DownloadFile(url: URL, file: File)

class DownloadActor(progressActor: ActorRef) extends Actor {
  def receive = {
    case DownloadFile(url, file) => {
      val writer = new FileOutputStream(file)
      var rd: DataInputStream= null
      val bufferSize = 2048 
      try {
        val conn = url.openConnection();
        val length = conn.getContentLength()
        rd = new DataInputStream(conn.getInputStream())
        progressActor ! Initialized(length)
        
        val bytes = Array.fill[Byte](bufferSize)(0)
        var bytesWrites = 0
        var bytesRead = rd.read(bytes)
        while (bytesRead != -1) {
          progressActor ! Update(bytesRead)
          val writeBytes = Array.ofDim[Byte](bytesRead)
          Array.copy(bytes, 0, writeBytes, 0, bytesRead)
          writer.write(writeBytes)
          bytesWrites += bytesRead
          bytesRead = rd.read(bytes)
        }
        progressActor ! Finished
        sender ! Right(file) 
      } catch {
        case e: IOException => {
          progressActor ! Failed
          sender ! Left(e)
        }
      } finally {
        if (rd != null) rd.close()
        if (writer != null) writer.close()
      }
    }
  }
}

private[adept] object Client extends Logging {
  
  def download(url: URL, file: File, progressActor: ActorRef)(implicit timeout: Timeout, system: ActorSystem): Future[Either[Exception, File]] = {
    logger.trace("client is fetching "+url+" to "+file+"..")
    
    val downloader = system.actorOf(Props(new DownloadActor(progressActor)))
    import akka.pattern.ask
    ask(downloader, DownloadFile(url, file)).mapTo[Either[Exception, File]]
  }
  
}