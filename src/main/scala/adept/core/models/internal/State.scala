package adept.core.models.internal

import adept.core.models._


case class State(
  val underconstrained: Set[Id],
  val overconstrained: Set[Id],
  val resolved: Set[Id],
  val resolvedVariants: Map[Id, Variant],
  val implicitVariants: Map[Id, Variant],
  val constraints: Map[Id, Set[Constraint]]) {

  def isResolved = underconstrained.size == 0 && overconstrained.size == 0
  def isUnderconstrained = underconstrained.size > 0 && overconstrained.size == 0
  def isOverconstrained = overconstrained.size > 0

  override def toString = {
    "resolved: " + resolved + "\n" +
      "over-constrained: " + overconstrained + "\n" +
      "under-constrained: " + underconstrained + "\n" +
      "resolved-variants: " + resolvedVariants + "\n" +
      "implicit-variants: " + implicitVariants + "\n" +
      "constraints: " + constraints
  }
}


///* 
// * State is mutable which speeds up the Resolver and makes the code easier to read. 
// * TODO: I am not sure we even need to have vars here now...
// * 
// * This will have to be rewritten to make it: 
// *  1) thread-safe for external libraries 
// *  2) multi-threaded to speed up IO (in particular) and CPU bounded operations 
// * */
//private[adept] class State(
//  var nodes: Map[String, Node] = Map.empty,
//  var graph: Set[Node] = Set.empty,
//  var visited: Set[Set[Variant]] = Set.empty,
//
//  var resolved: Set[String] = Set.empty,
//  var overconstrained: Set[String] = Set.empty,
//  var underconstrained: Set[String] = Set.empty,
//  var forcedVariants: Map[String, Variant] = Map.empty,
//  var resolvedVariants: Map[String, Variant] = Map.empty,
//
//  var optimalUnderconstrainedStates: Set[State] = Set.empty,
//
//  var constraints: Map[String, Set[Constraint]] = Map.empty) {
//  override def toString = {
//    var printedIds = Set.empty[String]
//    def nodesToString(nodes: Set[Node], level: Int): String = {
//      nodes.foreach(printedIds += _.id)
//      nodes.map { n =>
//        val (cyclic, nonCyclic) = n.children.partition(n => printedIds(n.id))
//        val cyclicString = cyclic.map(n => (" " * (level + 1)) + "- " + n.id + " <defined>").mkString("\n")
//        val nonCyclicString = nodesToString(nonCyclic, level + 1)
//        (" " * level) + "- " + resolvedVariants(n.id) + (if (cyclicString.isEmpty) "" else "\n" + cyclicString + "") + (if (nonCyclicString.isEmpty) "" else ("\n" + nonCyclicString))
//      }.mkString("\n")
//    }
//
//    "resolved: " + resolved + "\n" +
//      "over-constrained: " + overconstrained + "\n" +
//      "under-constrained: " + underconstrained + "\n" +
//      "resolved-variants: " + resolvedVariants + "\n" +
//      "forced-variants: " + forcedVariants + "\n" +
//      "constraints: " + constraints + "\n" +
//      "graph:\n" + nodesToString(graph, 0) + "\n" +
//      "optimal-underconstrained-states: " + optimalUnderconstrainedStates
//  }
//
//  def copy(
//      forcedVariants: Map[String, Variant] = (forcedVariants.keys zip forcedVariants.values).toMap //FIXME: hack to make a new copy of values
//  ) = {
//    new State(
//      nodes = (nodes.keys zip nodes.values).toMap, //FIXME: hack to make a new copy of values
//      graph = graph,
//      visited = visited,
//      resolved = resolved,
//      underconstrained = underconstrained,
//      overconstrained = overconstrained,
//      resolvedVariants = (resolvedVariants.keys zip resolvedVariants.values).toMap, //FIXME: hack to make a new copy of values
//      optimalUnderconstrainedStates = optimalUnderconstrainedStates,
//      forcedVariants = forcedVariants,
//      constraints = (constraints.keys zip constraints.values).toMap) //FIXME: hack to make a new copy of values
//  }
//}