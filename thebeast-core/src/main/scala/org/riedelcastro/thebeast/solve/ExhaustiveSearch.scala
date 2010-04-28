package org.riedelcastro.thebeast.solve


import env._
import doubles.DoubleTerm
import reflect.Manifest
import util.{Trackable, Util}
/**
 * @author Sebastian Riedel
 */

object ExhaustiveSearch extends ArgmaxSolver with SatisfiabilitySolver with Trackable {
  def argmax(term: DoubleTerm) = {
    val y = search(term.asInstanceOf[Term[Double]], (x: Double, y: Double) => x > y, Math.MIN_DOUBLE)
    ArgmaxResult(y, Status.Solved, y(term))
  }

  def satisfy(term: Term[Boolean]) =
    search(term.asInstanceOf[Term[Boolean]], (x: Boolean, y: Boolean) => x && !y, false)

  def search[T](term: Term[T])(implicit m: Manifest[T]): Env = {
      m.toString match {
        case "int" => search(term.asInstanceOf[Term[Int]], (x: Int, y: Int) => x > y, Math.MIN_INT)
        case "double" => search(term.asInstanceOf[Term[Double]], (x: Double, y: Double) => x > y, Math.MIN_DOUBLE)
        case "boolean" => search(term.asInstanceOf[Term[Boolean]], (x: Boolean, y: Boolean) => x && !y, false)
        case _ => null
      }
  }

  def search[T](term: Term[T], larger: (T, T) => Boolean, init: T): Env = {
    |**("Exhaustive Search for " + term)
    val env = new MutableEnv
    var max: T = init
    var best = new MutableEnv

    Env.forall(term.variables){
      env => {
        val result = env(term);
        if (max == null || larger(result, max)) {
          max = result
          best = env.clone
        }
      }
    }
    **|
    best
  }

}