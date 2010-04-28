package org.riedelcastro.thebeast.env.doubles

import collection.mutable.HashMap
import org.riedelcastro.thebeast.env.vectors.{Vector, VectorDotApp, VectorVar, VectorTerm}
import org.riedelcastro.thebeast.solve.ExhaustiveMarginalInference
import org.riedelcastro.thebeast.env._


/**
 */
case class LogLinear(sufficient: VectorTerm, weights: VectorVar, bias: DoubleTerm)
        extends Exp(Sum(Seq(VectorDotApp(sufficient, weights), bias))) {
  def marginalizeLogLinear(incoming: Beliefs[Any, EnvVar[Any]], weightsValue: Vector): Beliefs[Any, EnvVar[Any]] = {
    //default implementation
    val env = new MutableEnv
    //set weight variables in environment
    env(weights) = weightsValue
    //create the grounded term (that doesn't have weight variables)
    val grounded = ground(env)
    //exhaustive inference
    ExhaustiveMarginalInference.marginalizeQueries(grounded, incoming, Set(sufficient))
  }

}

object LogLinearMatch {
  def unapply(term: DoubleTerm): Option[(VectorTerm, VectorVar, DoubleTerm)] = term match {
    case Exp(Sum(Seq(VectorDotApp(sufficient, weights: VectorVar), bias: DoubleTerm))) => Some((sufficient, weights, bias))
    case Exp(Sum(Seq(VectorDotApp(weights: VectorVar, sufficient), bias: DoubleTerm))) => Some((sufficient, weights, bias))
    case Exp(VectorDotApp(sufficient, weights: VectorVar)) => Some((sufficient, weights, DoubleConstant(0.0)))
    case Exp(VectorDotApp(weights: VectorVar, sufficient)) => Some((sufficient, weights, DoubleConstant(0.0)))
    case LogLinear(sufficient, weights, bias) => Some((sufficient, weights, bias))
    case _ => None
  }
}

object LogLinearLikelihoodMatch {
  def unapply(term: DoubleTerm): Option[(VectorTerm, VectorVar, Set[EnvVar[_]],Seq[Env])] = term match {
    case Objective(theta, ProdOverGroundings(Normalize(Objective(hidden, LogLinearMatch(feature, weights, _))), data))
      if (Set(weights) == theta) => Some((feature,weights,hidden,data))
    case _ => None
  }
}

/**
 * A Featurized term is a term that deterministically depends on
 * the value of a feature-vector * weight dot product. Very close
 * to general linear models, but does not require normalization.
 */
trait Featurized extends DoubleTerm {
  /**
   * The feature vector for the given world/env.
   */
  def features(env: Env): Vector

  /**
   * The means/expectations of features given some beliefs for the
   * free variable in the term, and assuming that these beliefs are independent.  
   */
  def means(incoming: Beliefs[Any, EnvVar[Any]]): Vector

  /**
   * The weight vector term.
   */
  def weights: VectorTerm
}

class Weights extends HashMap[VectorVar, Vector]
