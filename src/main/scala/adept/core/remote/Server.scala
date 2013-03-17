package adept.core.remote

import scala.slick.session.Database
import akka.actor._
import spray.can.server.SprayCanHttpServerApp
import spray.http._
import spray.http.HttpMethods._
import spray.http.MediaTypes._
import java.io.File

class AdeptService(db: Database) extends Actor {
  private def zip(files: Seq[File]): Seq[File] = {
    null
  }
  
  val jsonExample = HttpResponse(
    entity = HttpBody(`application/json`,
      """{"foo":"test"}"""
    ))
  
  def receive = {
    case HttpRequest(GET, "/", _, _, _) => sender ! jsonExample
  }
}

class Server(db: Database, port: Int) extends SprayCanHttpServerApp {
  
  val server =  newHttpServer(system.actorOf(Props(new AdeptService(db))))

  def start: Unit = {
    server ! Bind(interface = "localhost", port = port)
  }
  
  def stop: Unit = {
     system.shutdown() 
  }
}