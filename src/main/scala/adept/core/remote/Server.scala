package adept.core.remote

import scala.slick.session.Database
import akka.actor._
import spray.can.server.SprayCanHttpServerApp
import spray.http._
import spray.http.HttpMethods._
import spray.http.MediaTypes._
import java.io._
import adept.core.models._
import adept.core.models._
import util._
import adept.core.operations.Queries
import adept.core.operations.Merge
import java.util.zip._
import com.sun.xml.internal.messaging.saaj.util.ByteOutputStream
import spray.httpx.encoding.GzipCompressor
import org.h2.tools.Backup
import scala.slick.session.Session
import com.typesafe.scalalogging.slf4j.Logging
import scala.collection.mutable.ArrayBuffer
import spray.util.SprayActorLogging

object AdeptService {
  def changesPrefix(repoName: String) = "/" + repoName+ "/changes"
  def clonePrefix(repoName: String) = "/" + repoName+ "/clone"
}

private[remote] class AdeptService(mainDB: Database, repoName: String) extends Actor with Logging with SprayActorLogging {
  override val log = akka.event.NoLogging
  import AdeptService._
  
  def jsonResponse(jsonString: String) = 
    HttpResponse(status = StatusCodes.Accepted, 
      entity = HttpBody(`application/json`,
          jsonString
        )
      )

      
  def receive = {
    case HttpRequest(GET, uri, headers, entity, protocol) if uri.startsWith(clonePrefix(repoName)) => {
      val zipFile = File.createTempFile("adept-"+repoName + "-backup-", ".zip")
      try {
        logger.trace(s"zipping to ${zipFile.getAbsolutePath}")
      
        //TODO: no this is not right: we are BLOCKING like hell here! ok only in POC
        mainDB.withSession{ implicit session: Session =>
          import scala.slick.jdbc.{StaticQuery => Q}
          Q.updateNA(s"""BACKUP TO '${zipFile.getAbsolutePath}'""").execute
        }
        //TODO: AGAIN! not right!! blocking on IO, we should be streaming here...
        val dis = new DataInputStream(new FileInputStream(zipFile));
        val buffer= new Array[Byte](2048)
        val bytes = new ArrayBuffer[Byte]()
        var bytesRead = dis.read(buffer)
        while(bytesRead != -1) {
          bytes ++= buffer
          bytesRead = dis.read(buffer)
        }
        //TODO: we should have a response/request pattern where the client does this instead:
        //- ask for last commit
        //- download last commit
        logger.trace(s"sending ${bytes.length} bytes...")
        sender ! HttpResponse(
            status = StatusCodes.Accepted,
            entity = HttpBody(`application/zip`, bytes.toArray) //TODO: do this some where else where you can stream!
           )
      } finally {
        zipFile.delete() //TODO: cache instead of deleting?
      }
    }
    case HttpRequest(GET, uri, headers, entity, protocol) if uri.startsWith(changesPrefix(repoName)) => {
      val HashRegExp = s"""${changesPrefix(repoName)}/(.*?)""".r
        
      uri match {
        case HashRegExp(hashString) => {
          val perhapsChangeSets = Merge.findChanges(Hash(hashString), mainDB)
          if (perhapsChangeSets.isFailure) { //TODO: how should this be done agian?
             sender ! jsonResponse(s"""{"message":"${perhapsChangeSets.failed.get.getMessage()}"}""")
          } else {
            val changeSets = perhapsChangeSets.get
            import org.json4s.JsonDSL._
            import org.json4s.native.JsonMethods._
            sender ! jsonResponse(compact(render(changeSets.map(_.toJson))))
          }
        }
        case unMatchingUri => {
          sender ! jsonResponse(s"""{"message":"could find hash in uri: $unMatchingUri. Format must be ${changesPrefix(repoName)}/<hash>"}""")
        }
      }
    }
    case request @ HttpRequest(method, uri, headers, entity, protocol) => {
      sender ! jsonResponse(s"""{"message":"could not find a corresponding route for request", "request": "$request"}""")
    } 
      
  }
}

private[core] object Server {
  import adept.core.db._
  import adept.core.db.DAO.driver.simple._
  import adept.core.db.Types._
  
  //TODO: remove either db or dbFiles both are not needed?
  def apply(db: Database, repoName: String) = new Server(db, repoName)
}

private[core] class Server private(mainDB: Database, repoName: String) extends SprayCanHttpServerApp {
  
  val server =  newHttpServer(system.actorOf(Props(new AdeptService(mainDB, repoName))))

  def start(port: Int): Unit = {
    server ! Bind(interface = "localhost", port = port)
  }
  
  def stop: Unit = {
     system.shutdown() 
  }
}