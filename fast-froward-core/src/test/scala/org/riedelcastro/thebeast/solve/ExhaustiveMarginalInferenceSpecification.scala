package org.riedelcastro.thebeast.solve

import org.riedelcastro.thebeast._
import env.{FunAppVar, Values, TheBeastEnv}
import org.specs.runner.JUnit4
import org.specs.Specification

/**
 * @author Sebastian Riedel
 */
class ExhaustiveMarginalInferenceTest extends JUnit4(ExhaustiveMarginalInferenceSpecification)

object ExhaustiveMarginalInferenceSpecification extends Specification with TheBeastEnv {
  "Exhaustive Marginal Inference" should {
    "find the exact marginals" in {
      val Persons = Values("Mika", "Sebastian")
      val friends = "friends" <~ (Persons x Persons) -> Bools
      val model = sum(Persons) {p => $(friends("Mika", p)) * 2.0} +
              sum(Persons) {p => $(friends(p, "Sebastian")) * 1.0} +
              $(friends("Sebastian", "Mika")) * 3.0
      val beliefs = ExhaustiveMarginalInference.infer(model)
      //Mika and Sebastians are friends in 8 worlds.
      //MS MM SM SS
      //1  0  0  0  2.0 + 1.0 = 3
      //1  0  0  1  2.0 + 1.0 + 1.0 = 4
      //1  0  1  0  2.0 + 1.0 + 3.0 = 6
      //1  0  1  1  2.0 + 1.0 + 3.0 + 1.0 = 7
      //first half: 20, second half: 20 + 4 * 2.0 = 48
      //false 48 - 8 * 3.0 = 24
      beliefs.belief(FunAppVar(friends, ("Mika", "Sebastian"))).belief(true) must_== 48.0
      beliefs.belief(FunAppVar(friends, ("Mika", "Sebastian"))).belief(false) must_== 24.0
    }
  }

}