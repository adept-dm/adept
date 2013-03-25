package adept.core.remote

import scala.slick.session.Database
import akka.actor._
import spray.can.server.SprayCanHttpServerApp
import spray.http._
import spray.http.HttpMethods._
import spray.http.MediaTypes._
import java.io.File
import adept.core.models._
import adept.core.models._
import util._
import adept.core.operations.Queries
import adept.core.operations.Merge

object AdeptService {
  def changesPrefix(repoName: String) = "/" + repoName+ "/changes"
}

private[remote] class AdeptService(mainDB: Database, repoName: String) extends Actor {
  import AdeptService._
  
  def jsonResponse(jsonString: String) = 
    HttpResponse(status = StatusCodes.BadRequest, 
      entity = HttpBody(`application/json`,
          jsonString
        )
      )

  def receive = {
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