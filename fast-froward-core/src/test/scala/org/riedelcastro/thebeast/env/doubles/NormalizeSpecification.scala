package org.riedelcastro.thebeast.env.doubles


import booleans.{NotApp, Disjunction}
import specs.runner.JUnit4
import specs.Specification

/**
 * @author Sebastian Riedel
 */

class NormalizeTest extends JUnit4(NormalizeSpecification)
object NormalizeSpecification extends Specification with TheBeastEnv {
  "An Normalize Term" should {
    "evaluate to a value that equals its evaluated argument divided by the sum of the " +
            "evaluations for all possible bindings of free variables in the argument term " in {
      val x = DoubleVar("x",Values(1.0,2.0,3.0))
      val binding = new MutableEnv
      binding.mapTo(x)->1.0
      binding(normalize(x)) must_== 1.0 / 6.0
    }
    "evaluate to 1.0 for an argument without free variables" in {
      val binding = new MutableEnv
      binding(normalize(6.0)) must_== 1.0
    }
  }

}