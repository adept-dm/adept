package adept.core

import akka.actor.ActorSystem

object ExecutionContexts {
  lazy val IOExecutionContext = ActorSystem("adept").dispatchers.lookup("io-dispatcher") 
}