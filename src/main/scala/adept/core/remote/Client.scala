package adept.core.remote

import spray.http._
import spray.util._
import spray.can.client._
import com.typesafe.scalalogging.slf4j.Logging
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import java.net.URL
import adept.core.models._
import scala.concurrent._
import scala.util._
import java.io.File
import scala.util.{Success, Failure}
import scala.concurrent.duration._
import akka.actor.{Props, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import spray.httpx.RequestBuilding._
import spray.http._
import spray.util._
import java.io._

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
        sender ! Success(file) 
      } catch {
        case e: IOException => {
          progressActor ! Failed
          sender ! Failure(e)
        }
      } finally {
        if (rd != null) rd.close()
        if (writer != null) writer.close()
      }
    }
  }
}

private[core] object Client extends Logging {
  def download(url: URL, file: File, progressActor: ActorRef)(implicit timeout: Timeout, system: ActorSystem): Future[Try[File]] = {
    logger.trace(s"client is fetching $url to $file..")
    
    val downloader = system.actorOf(Props(new DownloadActor(progressActor)))
    import akka.pattern.ask
    ask(downloader, DownloadFile(url, file)).mapTo[Try[File]]
  }
  
  def fetch(fromHash: Hash, repoName: String, url: URL)(implicit system: ActorSystem): Future[Try[Seq[ChangeSet]]] = {
    logger.trace(s"fetching from $fromHash in $repoName in $url...")
    val httpClient = DefaultHttpClient(system)
    val path = if (url.getPath().isEmpty) "" else url.getPath() + "/"
    val responseFuture =
      HttpDialog(httpClient, url.getHost, port = url.getPort)
        .send(HttpRequest(uri = AdeptService.changesPrefix(repoName) + "/" + fromHash))
        .end
    responseFuture.map{ response =>
      import org.json4s.native.JsonMethods._
      logger.trace(s"got ${pretty(render(parse(response.entity.asString)))}")
      Try(ChangeSet.fromJson(parse(response.entity.asString)))
    }
  }
}