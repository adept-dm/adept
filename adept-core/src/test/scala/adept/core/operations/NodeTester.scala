package adept.core.operations.graph

import org.scalatest._
import adept.core.operations._
import adept.core.models._
import akka.actor.Actor
import adept.core.models.Module
import akka.actor.ActorSystem
import akka.actor.Props
import scala.concurrent.blocking
import akka.actor.ActorRef

case class Load(configs: Set[Configuration], stack: Seq[Module])

class Node(module: Module, configurationResolver: ActorRef) extends Actor {
  def receive = {
    case Load(configs, stack) =>

      val dependencies = Set.empty[Dependency] 

      dependencies.foreach { dependency =>
        val circularDependency = false //check stack if module exists: means a circular dependencies
        if (circularDependency)
          //self ! EvictedDependency(reason = "circular dependency")
          null
        else
          configurationResolver ! ResolveDependency(dependency, self)
      }

    case evictedModule: EvictedModule =>
      //store 
      
    case DependencyMediationComplete(module) => 
      conflictResolver ! module
      
    case ConflictResolutionComplete(module) =>
      //store child
  }
}

case class ResolveDependency(dependency: Dependency, node: ActorRef)
class ConfigurationResolver extends Actor {
  def receive = {
    case ResolveDependency(dependency, node) =>
      moduleLoader ! Retrieve(dependency.coordinates, dependency.uniqueId, node)
    case RetrievedModule(module, node) =>
      val evicted = false
      if (evicted) {
        val reason = ""
        node ! EvictedModule(module, reason)
      } else {
        dependencyMediator ! (module, node)
      }
  }
}

case class MediateDependency(module: Module, node: ActorRef)
class DependencyMediator extends Actor {

  def receive = {
    case MediateDependency(module, node) =>
      //store module key and pass the key to the  
      //
      

    //store coordinates
      
   

  }
}

class ConflictResolver extends Actor {
  def receive = {
    case f => 
      
  }
}

case class Retrieve(coords: Coordinates, maybeUniqueId: Option[UniqueId], node: ActorRef)
case class RetrievedModule(module: Module, node: ActorRef)
class ModuleLoader extends Actor {
  def receive = {
    case Retrieve(coordinates, maybeUniqueId, node) =>
      val module: Module = {
        val cached = true //TODO
        if (cached) null //TODO
        else {
          blocking {
            //load module from disk
            null // TODO
          }
        }
      }
      sender ! RetrievedModule(module, node)
  }
}

class NodeTester extends FunSuite with MustMatchers {

  test("basic tests") {
    import adept.core.tests.TestData._

    val system = ActorSystem("test")

    val moduleLoader = system.actorOf(Props(new ModuleLoader()))
    val node = system.actorOf(Props(new Node(adept10, moduleLoader)))

    //want to be able to choose what I end up with: artifacts or modules
    //want to be lazy: only load things that is needed
    //want to reutilize cache in all projects
    //want be faster
    //want to have the same eviction rules
    //want to be more extensible
    //want to be able to specify what to do and overload it (hooks)

    //define what to do at which phase: 

    //buildGraph.withDefaults.artifacts
    
    node ! Load(Set())

  }

}