package org.riedelcastro.thebeast.solve


import env._
import doubles.{DoubleTerm, Sum}
import vectors.VectorDotApp

/**
 * @author Sebastian Riedel
 */

trait FactorGraphArgmaxSolver extends ArgmaxSolver {

  type FactorGraphType <: DoubleFactorGraph

  def solve(): ArgmaxResult;

  def getFactorGraph() : FactorGraphType
  def createFactorGraph() : FactorGraphType

  def argmax(term: DoubleTerm) = {
    term match {
      case x: Sum[_] => {
        val graph = createFactorGraph
        graph.addTerms(x.args)
        solve();
      }
      case x: VectorDotApp => argmax(x.distribute)
      case _ => ArgmaxResult(null, Status.CantDo, 0)
    }
  }
}