package org.riedelcastro.thebeast.alchemy


import java.io.StringReader
import org.specs.Specification
import org.specs.runner.JUnit4
import org.riedelcastro.thebeast.env._
import vectors.QuantifiedVectorSum

/**
 * @author Sebastian Riedel
 */

class MLNTest extends JUnit4(MLNSpecification)

object MLNSpecification extends Specification with TheBeastEnv with AlchemySmokingFixtures {
  "An MLN" should {
    "be extended by predicates and types when loading an alchemy file with the corresponding predicate definitions" in {
      val mln = new MLN
      mln.loadMLN(new StringReader(smokingSignature))
      mln.getPredicate("Friends").values.domain.arity must_== 2
      mln.getPredicate("Smokes").values.domain.arity must_== 1
      mln.getPredicate("Cancer").values.domain.arity must_== 1
      mln.getType("person").size must_== 0
    }
    "load the formulae of an alchemy file and create equivalent formulae in scala" in {
      val mln = new MLN
      mln.loadMLN(new StringReader(smokingSignature + smokingRules))
      val person = mln.getType("person")
      val smokes = mln.getPredicate("Smokes")
      val cancer = mln.getPredicate("Cancer")
      val friends = mln.getPredicate("Friends").asInstanceOf[Predicate[Tuple2[String, String]]]
      val x = Var("x", person).asInstanceOf[Var[String]]
      val y = Var("y", person).asInstanceOf[Var[String]]
      mln.getFormula(0) must_==
              QuantifiedVectorSum(x, $$(smokes(x) ~> cancer(x)) * unit("F0"))
      mln.getFormula(1) must_==
              QuantifiedVectorSum(x, QuantifiedVectorSum(y, $$(friends(x, y) ~> (smokes(x) <~> smokes(y))) * unit("F1")))
    }
    "load the weights of formulae in an alchemy file" in {
      val mln = new MLN
      mln.loadMLN(new StringReader(smokingSignature + smokingRules))
      mln.getWeights.get("F0") must_== 0.0
      mln.getWeights.get("F1") must_== 0.0
    }

    "load atoms from an alchemy database file" in {
      val mln = new MLN
      mln.loadMLN(new StringReader(smokingSignature + smokingRules))
      val smokes = mln.getPredicate("Smokes")
      val friends = mln.getPredicate("Friends").asInstanceOf[Predicate[Tuple2[String, String]]]
      val env = mln.loadAtoms(new StringReader(smokingTrainData))
      env.close(smokes, true)
      env(smokes("Anna")) must_== true
      env(smokes("Helen")) must_== false
      env(friends("Edward", "Anna")) must_== true


    }

  }


}