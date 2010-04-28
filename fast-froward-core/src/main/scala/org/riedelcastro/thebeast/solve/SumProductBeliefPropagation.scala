package org.riedelcastro.thebeast.solve


import org.riedelcastro.thebeast._
import env._
import doubles._
import util.{Logging, Trackable}
/**
 * @author Sebastian Riedel
 */

class SumProductBeliefPropagation extends MarginalInference with Trackable with Logging {
  class SPBPFactorGraph extends DoubleFactorGraph {
    case class SPBPEdge(override val node: NodeType, override val factor: FactorType) extends Edge(node, factor) {
      var node2factor: Belief[Any] = Ignorance(node.variable.values)
      var factor2node: Belief[Any] = Ignorance(node.variable.values)

      def updateNode2Factor = {
        node2factor = (node.belief / factor2node).normalize
      }

    }

    case class SPBPNode(override val variable: EnvVar[Any]) extends Node(variable) {
      var belief: Belief[Any] = Ignorance(variable.values)

      def updateBelief = {
        val old = belief
        belief = edges.foldLeft[Belief[Any]](Ignorance(variable.values)) {
          (r, e) => r * e.factor2node
        }.normalize
        (old - belief).norm
      }

    }

    case class SPBPFactor(override val term: TermType) extends Factor(term) {
      def updateOutgoingMessages = {

        |**("Term marginalization")
        val incomingBeliefs = new MutableBeliefs[Any, EnvVar[Any]]
        for (edge <- edges) incomingBeliefs.setBelief(edge.node.variable, edge.node2factor)
        val outgoingBeliefs = term.marginalize(incomingBeliefs)
        **|

        |**("Divide by incoming message and normalize")
        for (edge <- edges) edge.factor2node = (outgoingBeliefs.belief(edge.node.variable) / edge.node2factor).normalize
        **|
      }
    }



    type FactorType = SPBPFactor
    type NodeType = SPBPNode
    type EdgeType = SPBPEdge

    protected def createFactor(term: TermType) = SPBPFactor(term)

    protected def createNode(variable: EnvVar[_]) = SPBPNode(variable)

    protected def createEdge(node: NodeType, factor: FactorType) = SPBPEdge(node, factor)

    def updateMessages(): Double = {
      //synchronous edge processing
      |**("Update factor to node messages")
      for (factor <- factors)
        factor.updateOutgoingMessages
      **|

      |**("Update beliefs")
      var maxChange = 0.0
      for (node <- nodes)
        maxChange = Math.max(node.updateBelief, maxChange)
      **|

      |**("Update node to factor messages")
      for (edge <- edges)
        edge.updateNode2Factor
      **|

      maxChange
    }
  }



  def infer(term: DoubleTerm) = term match {
    case Normalize(x) => infer(Factorizer.toMultiplication(x).args).normalize
    case x => infer(Factorizer.toMultiplication(x).args)
  }

  private var _iterations = 0

  def iterations = _iterations

  private def infer(terms: Collection[DoubleTerm]): Beliefs[Any, EnvVar[Any]] = {

    debug("SPBP for %d terms: %s".format(terms.size,terms.mkString(",")))

    val graph = new SPBPFactorGraph

    |**("Constructing graph")
    graph.addTerms(terms.map(DoubleTermOptimizer.optimize(_)))
    **|

    debug("%d BP Nodes: %s".format(graph.nodes.size, graph.nodes.mkString(",")))

    _iterations = 0
    |**("Message passing")
    while (graph.updateMessages > 0.0001) {_iterations += 1}
    **|

    val result = new MutableBeliefs[Any, EnvVar[Any]]
    for (node <- graph.nodes)
      result.setBelief(node.variable, node.belief)

    result
  }
}