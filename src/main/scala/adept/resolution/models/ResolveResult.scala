package adept.resolution.models

import adept.models.Id

/**
 * ADT to represent the Resolved results.
 * 
 * Always contains a graph (if not resolved, this graph is partial), 
 * with the root nodes, and `State`.
 * 
 * `ResolveResult` is immutable and thread-safe.
 */
sealed trait ResolveResult {
  val state: State
  val graph: Set[Node]

  def isResolved = state.isResolved
  
  def graphAsString: String = {
    var printedIds = Set.empty[Id]

    def nodesToString(nodes: Set[Node], level: Int): String = {
      nodes.foreach(printedIds += _.id)
      nodes.map { n =>
        val (cyclic, nonCyclic) = n.children.partition(n => printedIds(n.id))
        val cyclicString = cyclic.map(n => (" " * (level + 2)) + "- " + n.id + " <defined>").mkString("\n")
        val nonCyclicString = nodesToString(nonCyclic, level + 2)
        val variantString = (state.resolvedVariants.get(n.id) orElse state.implicitVariants.get(n.id)).map(v => " - " + v.toString) orElse (state.overconstrained.find(_ == n.id).map(i => " X " + i) orElse state.underconstrained.find(_ == n.id).map(i => " ? " + i))
        variantString.map { variantString =>
          (" " * level) + variantString + (if (cyclicString.isEmpty) "" else "\n" + cyclicString + "") + (if (nonCyclicString.isEmpty) "" else ("\n" + nonCyclicString))
        }.get
      }.mkString("\n")
    }

    nodesToString(graph, 0)
  }

  override def toString = {
    this.getClass.getSimpleName + "(\n" +
      state.toString.lines.map(l => "  " + l).mkString("\n") +
      "\n)" + (if (graph.nonEmpty) "[\n" + graphAsString + "\n]" else "")
  }
}

class ResolvedResult(override val state: State, override val graph: Set[Node]) extends ResolveResult
class UnderconstrainedResult(override val state: State, override val graph: Set[Node], val optimalStates: Set[State]) extends ResolveResult
class OverconstrainedResult(override val state: State, override val graph: Set[Node]) extends ResolveResult

