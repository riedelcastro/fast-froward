package org.riedelcastro.thebeast.solve

import org.riedelcastro.thebeast._
import env._
import env.doubles._
import util.{Trackable}

/**
 * @author Sebastian Riedel
 */

trait MarginalInference {
  def infer(term: DoubleTerm): Beliefs[Any, EnvVar[Any]]

}

object ExhaustiveMarginalInference extends MarginalInference with Trackable {

  //todo: a lot of code duplication here, get rid of this
  def marginalize(term: DoubleTerm, incoming: Beliefs[Any, EnvVar[Any]]): Beliefs[Any, EnvVar[Any]] = {
    val multiplied = term * Multiplication(term.variables.map(v => BeliefTerm(incoming.belief(v), v)).toSeq)
    ExhaustiveMarginalInference.infer(multiplied)
  }

  def marginalizeQueries[N](term: DoubleTerm,
                            incoming: Beliefs[Any, EnvVar[Any]],
                            queries: Collection[NumericTerm[N]]): Beliefs[Any, EnvVar[Any]] = {
    val multiplied = term * Multiplication(term.variables.map(v => BeliefTerm(incoming.belief(v), v)).toSeq)
    ExhaustiveMarginalInference.inferQueries(multiplied, queries)
  }

  def infer(term: DoubleTerm): Beliefs[Any, EnvVar[Any]] = term match {
    case Normalize(x) => inferExhaustively(x).normalize
    case _ => inferExhaustively(term)
  }

  def inferQueries[N](term: DoubleTerm, queries: Collection[NumericTerm[N]]): Beliefs[Any, EnvVar[Any]] = term match {
    case Normalize(x) => inferQueriesExhaustively(x, queries).normalize
    case _ => inferQueriesExhaustively(term, queries)
  }


  private def inferExhaustively(term: DoubleTerm): Beliefs[Any, EnvVar[Any]] = {
    val domain = term.variables.toSeq
    gatherBeliefs(term, domain, Seq.empty)
  }

  private def inferQueriesExhaustively[N](term: DoubleTerm,
                                          queries: Collection[NumericTerm[N]]): Beliefs[Any, EnvVar[Any]] = {
    val domain = term.variables.toSeq
    gatherBeliefs(term, domain, queries)
  }

  private def gatherBeliefs[N](term: DoubleTerm,
                               variables: Collection[EnvVar[Any]],
                               queries: Collection[NumericTerm[N]]): Beliefs[Any, EnvVar[Any]] = {
    |**("Exhaustive marginal inference for " + term)

    val beliefs = new MutableBeliefs[Any, EnvVar[Any]]

    Env.forall(variables) {
      env => {
        val score = env(term);
        for (variable <- variables) {
          beliefs.increaseBelief(variable, env(variable), score)
        }
        for (query <- queries) {
          beliefs.increaseExpectation(query, env(query), score)
        }
      }
    }

    **|
    beliefs

  }


}

object OptimizedMarginalizer {
  def marginalize(term: DoubleTerm, incoming: Beliefs[Any, EnvVar[Any]]): Beliefs[Any, EnvVar[Any]] = term match {
    case Exp(Multiplication(Seq(Indicator(x), y: DoubleTerm))) => null
    case x => defaultMarginalize(x, incoming)
  }

  def defaultMarginalize(term: DoubleTerm, incoming: Beliefs[Any, EnvVar[Any]]): Beliefs[Any, EnvVar[Any]] = {
    val productTerm = term * Multiplication(term.variables.map(v => BeliefTerm(incoming.belief(v), v)).toSeq)
    ExhaustiveMarginalInference.infer(productTerm)
  }
}