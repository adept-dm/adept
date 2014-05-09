package adept.ui

import akka.actor.{ ActorSystem, Props }
import akka.pattern.ask
import akka.io.IO
import spray.can.Http
import java.awt.Desktop
import java.net.URI
import scala.concurrent.Await
import akka.util._
import akka.actor.ActorRef

object Main extends App {

  implicit val system = ActorSystem()

  // the handler actor replies to incoming HttpRequests
  val handler = system.actorOf(Props[AdeptUIService], name = "handler")

  val Port = 8137
  import scala.concurrent.duration._
  implicit val timeout: Timeout = 5.seconds
  val actor: ActorRef = IO(Http)
  Await.result(ask(actor, Http.Bind(handler, interface = "localhost", port = Port)), timeout.duration) match {
    case result: akka.io.Tcp.CommandFailed =>
      system.shutdown()
      throw new Exception("Cannot bind to server on localhost port: " + Port + " so shutting down. Adept UI running somewhere else?")
    case _ =>
      if (Desktop.isDesktopSupported()) {
        Desktop.getDesktop().browse(new URI("http://localhost:" + Port));
      } else {
        throw new Exception("Cannot open browser")
      }
  }
}