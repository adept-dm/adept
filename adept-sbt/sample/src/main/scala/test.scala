import akka._

import play._

import akka.actor.Actor
import akka.actor.Props

object Greeter {
  case object Greet
  case object Done
}
 
class Greeter extends Actor {
  def receive = {
    case Greeter.Greet ⇒
      println("Hello World!")
      sender ! Greeter.Done
  }
} 

class HelloWorld extends Actor {
 
  override def preStart(): Unit = {
    // create the greeter actor
    val greeter = context.actorOf(Props[Greeter], "greeter")
    // tell it to perform the greeting
    greeter ! Greeter.Greet
  }
 
  def receive = {
    // when the greeter is done, stop this actor and with it the application
    case Greeter.Done ⇒ context.stop(self)
  }
}