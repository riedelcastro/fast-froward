package org.riedelcastro.thebeast.learn


import org.riedelcastro.thebeast.solve.{ExhaustiveSearch, ArgmaxSolver}
import org.riedelcastro.thebeast.env.vectors.{VectorTerm, Vector}
import org.riedelcastro.thebeast.util.Trackable
import org.riedelcastro.thebeast.env.{MutableEnv, DependsOn, MaskedEnv}
import org.riedelcastro.thebeast.env.doubles._

/**
 * @author Sebastian Riedel
 */

class OnlineLearner extends ArgmaxSolver with Trackable {
  var solver: ArgmaxSolver = ExhaustiveSearch
  var updateRule: UpdateRule = new PerceptronUpdateRule
  var maxEpochs: Int = 1


  def argmax(term: DoubleTerm): ArgmaxResult = {
    term match {
      case LogLinearLikelihoodMatch(feature,weights,hidden,data) => {
        val result = new MutableEnv
        result.set(weights,learn(feature,data.map(new MaskedEnv(_,hidden))))
        ArgmaxResult(result, Status.Solved, Math.NEG_INF_DOUBLE)
      }
      case _ => CantSolve
    }
  }

  def learn(featureVector: VectorTerm, trainingSet: Seq[MaskedEnv]): Vector = {
    |**("Learning");
    var weights = new Vector
    for (epoch <- 0 until maxEpochs) {
      for (instance <- trainingSet) {
        |**("Instance")
        var goldFeatures = instance.unmasked(featureVector)
        var conditionedScore = (featureVector dot weights).ground(instance)
        var guess = solver.argmax(conditionedScore).result
        var guessFeatures = instance.overlay(guess)(featureVector)
        updateRule.update(goldFeatures, guessFeatures, 0.0, weights)
        **|
      }
    }
    **|
    weights
  }

  trait UpdateRule {
    def update(gold: Vector, guess: Vector, loss: Double, weights: Vector)
  }

  class PerceptronUpdateRule extends UpdateRule {
    var learningRate = 1.0

    def update(gold: Vector, guess: Vector, loss: Double, weights: Vector) = {
      weights.addInPlace(gold.add(guess, -1.0), learningRate)
    }
  }
}

