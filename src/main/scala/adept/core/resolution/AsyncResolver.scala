package adept.core.resolution

import adept.core.models._
import akka.actor.{ Actor, ActorRef, ActorSystem }
import akka.util.Duration
import akka.dispatch.Await
import akka.actor.Props


/** NOT CURRENTLY USED. Just some ideas on how to improve thread safety and speed of the Resolver **/
object AsyncResolver {

  case class Load(dependencies: Seq[Dependency])
  case class Variants(variants: Set[Variant])

  case class AddConstraints(id: String, constraints: Set[Constraint])
  case class GetConstraints(id: String)
  case class Constraints(id: String, constraints: Set[Constraint])
  case class ConstraintsUpdated(id: String, constraints: Set[Constraint])

  case class Resolved(id: String)
  case class Unresolved(id: String)
  case class Completed(amount: Int)

  case class Child(child: ActorRef)

  class ConstraintsHandler extends Actor {
    var globalConstraints = Map.empty[String, Set[Constraint]]

    def receive = {
      case AddConstraints(id, constraints) =>
        val newConstraints = constraints ++ globalConstraints.get(id).flatten
        globalConstraints += id -> newConstraints
        nodeEventHandler ! ConstraintsUpdated(id, newConstraints)
      case GetConstraints(id) =>
        sender ! Constraints(id, globalConstraints.get(id).flatten.toSet)
    }
  }

  class NodeEventHandler(parent: ActorRef) extends Actor {
    var allNodes = Map.empty[String, ActorRef]

    var resolved = Set.empty[String]
    var unresolved = Set.empty[String]

    def completeIfFinished() = {
      if ((resolved.size + unresolved.size) == allNodes.size) parent ! Completed(allNodes.size)
    }

    def receive = {
      case Load(dependencies) => dependencies.foreach { dependency =>
        val id = dependency.id
        val node = allNodes.getOrElse(id, context.actorOf(Props(new Node(id, self))))
        allNodes += id -> node
        constraintsHandler ! AddConstraints(id, dependency.constraints)
        sender ! Child(node)
      }
      case ConstraintsUpdated(id, constraints) =>
        unresolved -= id
        resolved -= id
        allNodes(id) ! Constraints(id, constraints)
      case Resolved(id) =>
        resolved += id
        unresolved -= id
        completeIfFinished()
      case Unresolved(id) =>
        unresolved += id
        resolved -= id
        completeIfFinished()
    }
  }

  def resolveGraph(dependencies: Seq[Dependency]) = {
    import akka.pattern.ask
    val timeout = Duration("60 minutes")
    val completedFuture = ask(nodeEventHandler, Load(dependencies))(timeout).mapTo[Completed]
    val Completed(completed) = Await.result(completedFuture, timeout)
    println("resolved: " + completed + " variants")
  }
  
  class VariantsLoader(loaderEngine: VariantsLoaderEngine) extends Actor {
    def receive = {
      case Constraints(id, constraints) =>
        sender ! Variants(loaderEngine.get(id, constraints)) //TODO: cache
    }
  }

  class Node(id: String, parent: ActorRef) extends Actor {
    var variants = Set.empty[Variant]
    var children = Set.empty[ActorRef]

    def isResolved = variants.size == 1

    def receive = {
      case Constraints(id, constraints) =>
        variantsLoader ! Constraints(id, constraints)
      case Variants(foundVariants) =>
        variants = foundVariants
        if (isResolved) {
          nodeEventHandler ! Resolved(id)
          variants.foreach { variant =>
            nodeEventHandler ! Load(variant.dependencies)
          }
        } else nodeEventHandler ! Unresolved(id)
      case Child(child) => {
        children += child
      }
    }
  }

  lazy val system = ActorSystem()
  lazy val variantsLoader = system.actorOf(Props(new VariantsLoader(null)))

  lazy val nodeEventHandler = system.actorOf(Props(new NodeEventHandler(null)))
  lazy val constraintsHandler = system.actorOf(Props(new ConstraintsHandler))

}