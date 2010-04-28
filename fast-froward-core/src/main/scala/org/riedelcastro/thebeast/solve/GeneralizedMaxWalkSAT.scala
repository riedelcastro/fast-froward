package org.riedelcastro.thebeast.solve

import org.riedelcastro.thebeast._
import env._
import doubles.DoubleTerm
/**
 * @author Sebastian Riedel
 */

class GeneralizedMaxWalkSAT extends FactorGraphArgmaxSolver {
  type FactorGraphType = MWSFactorGraph

  class MWSFactorGraph extends DoubleFactorGraph {
    type NodeType = DoubleNode
    type FactorType = MWSFactor
    type EdgeType = Edge

    protected def createFactor(term: TermType) = new MWSFactor(term)
    protected def createNode(variable: EnvVar[_]) = new DoubleNode(variable)
    protected def createEdge(node: NodeType, factor: FactorType) = new Edge(node,factor)

    class MWSFactor(term:DoubleTerm) extends Factor(term) {

      def greedy(y:MutableEnv) : Double = {
        var bestChoice:(EnvVar[_],Any) = null
        var maxDelta = Math.NEG_INF_DOUBLE
        for (node <- edges.map(e => e.node)){
          val current = y.resolveVar(node.variable).get
          val oldScore = node.scoreNeighbours(y)
          for (value <- node.variable.values; if value != current){
            y += ((node.variable, value))
            val newScore = node.scoreNeighbours(y)
            val delta = newScore - oldScore
            if (delta > maxDelta) {
              maxDelta = delta
              bestChoice = (node.variable,value)
            }
          }
          y += ((node.variable, current))
        }
        y += ((bestChoice._1, bestChoice._2))
        maxDelta
      }

      def randomChange(y:MutableEnv) : Double = {
        val node = edges.randomValue.node
        val oldScore = node.scoreNeighbours(y)
        y += ((node.variable, node.variable.values.randomValue))
        val newScore = node.scoreNeighbours(y)
        newScore - oldScore
      }

    }

  }


  private val random = new scala.util.Random
  private var graph:MWSFactorGraph = new MWSFactorGraph

  val maxFlips = 1000


  def createFactorGraph() = {
    graph = new MWSFactorGraph
    graph
  }

  def getFactorGraph() = graph

  def solve(): ArgmaxResult = {

    val y = new MutableEnv
    //generate initial solution
    for (n <- graph.nodes) y += ((n.variable,n.variable.values.randomValue))

    //score
    var max = graph.sum(y)
    var score = max
    var best = y.clone

    for (flip <- 0 until maxFlips) {
      //find all terms not at their optimum => need to know what the max of a term is
      val suboptimal = graph.factors.filter(f => y(f.term) < f.term.upperBound)
      val violated = suboptimal(random.nextInt(suboptimal.size))
      score = score + (if (Math.random < 0.5) violated.randomChange(y) else violated.greedy(y))
      if (score > max){
        max = score
        best = y.clone
      }
    }

    ArgmaxResult(best, Status.Solved, max)
  }


}
