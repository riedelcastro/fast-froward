package org.riedelcastro.thebeast.solve

import org.riedelcastro.thebeast._
import env.doubles.DoubleTerm
import env.{Var, Env}

/**
 * @author Sebastian Riedel
 */

trait ArgmaxSolver {

  /**
   * Returns the possible world that maximizes the given term
   */
  def argmax(term:DoubleTerm) : ArgmaxResult

  
  //def argmax[T](variable:Var[T]) : T = null
  
  object Status extends Enumeration {
    type Status = Value
    val Solved, CantDo, Infeasible = Value
  }


  import Status._

  case class ArgmaxResult(val result:Env, val status:Status, val score:Double) {

  }

  object CantSolve extends ArgmaxResult(null, CantDo, Math.NEG_INF_DOUBLE)


}

