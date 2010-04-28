package org.riedelcastro.thebeast.env


import collection.mutable.HashMap
import doubles.{DoubleFunApp}

/**
 * @author Sebastian Riedel
 */

//todo: should we get rid of type parameters
trait Beliefs[V, T <: Term[V]] {
  def belief(term: T): Belief[V]

  def normalize: Beliefs[V, T]

  def terms: Collection[T]

  /**
   * Considers beliefs as fully factorized distribution over terms
   */
  def partitionFunction = terms.foldLeft(1.0) {(s, t) => s * belief(t).totalSum} 

  override def equals(obj: Any) = obj match {
    case that: Beliefs[_, _] => terms.forall(t => this.belief(t) == that.asInstanceOf[Beliefs[V, T]].belief(t))
    case _ => false
  }

  def termsWithExpectations: Collection[NumericTerm[Any]]

  def expectation[N](term:NumericTerm[N]):N 
}

class CompleteIgnorance[V,T<:Term[V]] extends Beliefs[V,T] {
  def expectation[N](term: NumericTerm[N]) = term.createExpectation.value

  def termsWithExpectations = Seq.empty

  def terms = Seq.empty

  def normalize = this

  def belief(term: T) = Ignorance(term.values)
}

//todo: should be +T but this needs changing of Expectation
trait NumericTerm[T] extends Term[T] {
  def createExpectation:Expectation[T]
}

class MutableBeliefs[V, T <: Term[V]] extends Beliefs[V, T] {
  private val beliefs = new HashMap[T, Belief[V]]
  private val expectations = new HashMap[NumericTerm[Any], Expectation[Any]]

  def setBelief(term: T, belief: Belief[V]) = {
    beliefs(term) = belief
  }

  def increaseBelief(term: T, value: V, delta: Double) = {
    var belief = beliefs.getOrElseUpdate(term,
                                        new MutableBelief[V](term.values)).asInstanceOf[MutableBelief[V]]
    belief.increaseBelief(value, delta)
  }

  def belief(term: T): Belief[V] = {
    beliefs(term)
  }

  def terms = beliefs.keySet

  def normalize = {
    val result = new MutableBeliefs[V, T]
    result.beliefs ++= beliefs.map(entry => (entry._1, entry._2.normalize))
    result
  }

  override def toString = beliefs.mkString("\n") + (if (expectations.size >0) "\n" + expectations.toString else "")

  def increaseExpectation[N](term:NumericTerm[N], value:N, prob:Double) = {
    val expectation = expectations.getOrElseUpdate(
      term.asInstanceOf[NumericTerm[Any]], term.createExpectation.asInstanceOf[Expectation[Any]])
    expectation.add(prob,value)
  }


  def expectation[N](term: NumericTerm[N]) = expectations(term.asInstanceOf[NumericTerm[Any]]).value.asInstanceOf[N]

  def termsWithExpectations = expectations.keySet
}

sealed trait Belief[T] {
  def values: Values[T]

  def belief(value: T): Double

  def normalize: Belief[T]

  def norm: Double = values.foldLeft(0.0) {(n, v) => {val b = belief(v); n + b * b}}

  def totalSum = values.foldLeft(0.0) {(s, v) => s + belief(v)} 

  def /(that: Belief[T]) = applyPointWiseIgnoreIgnorance(that, (b1, b2) => b1 / b2)

  def *(that: Belief[T]) = applyPointWiseIgnoreIgnorance(that, (b1, b2) => b1 * b2)

  def +(that: Belief[T]) = applyPointWise(that, (b1, b2) => b1 + b2)

  def -(that: Belief[T]) = applyPointWise(that, (b1, b2) => b1 - b2)

  def equals(that: Belief[T]) = values.forall(v => this.belief(v) == that.belief(v))

  override def equals(obj: Any) = obj match {
    case that: Belief[_] => values.forall(v => this.belief(v) == that.asInstanceOf[Belief[T]].belief(v))
    case _ => false
  }


  private def applyPointWiseIgnoreIgnorance(that: Belief[T], op: (Double, Double) => Double) = {
    that match {
      case Ignorance(_) => this
      case _ => {
        val result = new MutableBelief(values)
        for (v <- values) result.setBelief(v, op(belief(v), that.belief(v)))
        result
      }
    }
  }

  private def applyPointWise(that: Belief[T], op: (Double, Double) => Double) = {
    val result = new MutableBelief(values)
    for (v <- values) result.setBelief(v, op(belief(v), that.belief(v)))
    result
  }


}

case class BeliefTerm[T](belief: Belief[T], override val arg: Term[T])
    extends DoubleFunApp(Constant((t: T) => belief.belief(t)), arg) {
  override def toString = "Belief(" + arg + ")=" + belief
}

case class Ignorance[T](val values: Values[T]) extends Belief[T] {
  def belief(value: T) = 1.0

  override def *(that: Belief[T]) = that

  def normalize = this
}

//todo:  implement a immutable version
class MutableBelief[T](val values: Values[T]) extends Belief[T] {
  private val _belief = new HashMap[T, Double] {
    override def default(key:T) = 0.0
  }



  def belief(value: T) = _belief(value)

  def setBelief(value: T, belief: Double) = _belief(value) = belief

  def increaseBelief(value: T, belief: Double) = _belief(value) = _belief.getOrElse(value, 0.0) + belief

  def total = _belief.values.foldLeft(0.0) {(r, b) => r + b}

  def normalize = {
    val norm = total
    val result = new MutableBelief(values)
    for (pair <- _belief) {
      result._belief += pair._1 -> pair._2 / norm
    }
    result
  }

  override def toString = _belief.toString
}
