package org.riedelcastro.thebeast.env

import org.riedelcastro.thebeast._
import collection.mutable.{HashMap, ArrayBuffer}
import doubles.{DoubleConstant, DoubleTerm, SumHelper}
import util._

trait FactorGraph extends ListenerManager with Logging {
  type TermType <: Term[_]
  type NodeType <: Node
  type FactorType <: Factor
  type EdgeType <: Edge

  private val _factors = new ArrayBuffer[FactorType] with RandomDrawable[FactorType]
  private val _nodes = new ArrayBuffer[NodeType] with RandomDrawable[NodeType]
  private val _edges = new ArrayBuffer[EdgeType] with RandomDrawable[EdgeType]
  private val _term2Factor = new HashMap[TermType, FactorType]
  private val _variable2Node = new HashMap[EnvVar[_], NodeType]

  def factors: RandomDrawable[FactorType] = _factors

  def nodes: RandomDrawable[NodeType] = _nodes

  def edges: RandomDrawable[EdgeType] = _edges


  case class Factor(val term: TermType) {
    val edges = new ArrayBuffer[EdgeType] with RandomDrawable[EdgeType]

    def addEdge(edge: EdgeType) = edges += edge
  }
  case class Node(val variable: EnvVar[Any]) {
    val edges = new ArrayBuffer[EdgeType] with RandomDrawable[EdgeType]

    def addEdge(edge: EdgeType) = edges += edge
  }

  case class Edge(val node: NodeType, val factor: FactorType)

  sealed trait Event
  case class AddNodeEvent(node: NodeType) extends Event
  case class AddFactorEvent(factor: FactorType) extends Event

  type EventType = Event

  protected def createFactor(term: TermType): FactorType

  protected def createNode(variable: EnvVar[_]): NodeType

  protected def createEdge(node: NodeType, factor: FactorType): EdgeType

  def addTerm(t: TermType): FactorType = {
    var factorAdded = false;
    val factor = _term2Factor.getOrElseUpdate(t,
      {val f = createFactor(t); _factors += f; factorAdded = true; f})
    for (v <- t.variables) {
      var nodeAdded = false;
      val node = _variable2Node.getOrElseUpdate(v,
        {val n = createNode(v); _nodes += n; nodeAdded = true; n})
      val edge = createEdge(node, factor)
      _edges += edge
      node.addEdge(edge)
      factor.addEdge(edge)
      if (nodeAdded) fireEvent(AddNodeEvent(node))

    }
    if (factorAdded) fireEvent(AddFactorEvent(factor))
    factor
  }

  def addTerms(terms: Iterable[TermType]) = for (t <- terms) addTerm(t)

}

trait DoubleFactorGraph extends FactorGraph {
  type TermType = DoubleTerm

  class DoubleNode(override val variable: EnvVar[_]) extends Node(variable) {
    def scoreNeighbours(env: Env) = {
      edges.map(e => e.factor).foldLeft(0.0) {(s, f) => s + env(f.term)}
    }
  }

  def sum(env: Env) = SumHelper.sum(factors.map(f => f.term), env)

}

class TestFactorGraph extends DoubleFactorGraph {
  type FactorType = Factor
  type NodeType = Node
  type EdgeType = Edge

  protected def createFactor(term: TermType) = new Factor(term)

  protected def createNode(variable: EnvVar[_]) = new Node(variable)

  protected def createEdge(node: NodeType, factor: FactorType) = new Edge(node, factor)
}

object TestNewFG extends Application {
  val fg = new TestFactorGraph
  fg.addListener(e => e match {
    case fg.AddFactorEvent(factor) => println(factor)
    case _ => println("Don't know")
  })
  fg.addTerm(new DoubleConstant(1.0))


}