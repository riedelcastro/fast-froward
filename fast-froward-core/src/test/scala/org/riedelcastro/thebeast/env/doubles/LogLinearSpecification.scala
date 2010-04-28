package org.riedelcastro.thebeast.env.doubles

import org.specs.runner.JUnit4
import org.specs.Specification
import org.riedelcastro.thebeast.env._
import vectors.{Vector, VectorVar}

/**
 * @author Sebastian Riedel  
 */

class LogLinearTest extends JUnit4(LogLinearSpecification)
object LogLinearSpecification extends Specification {
  import TheBeastImplicits._
  "A LogLinear term" should {
    "calculate the expectation of its sufficient statistics" in {
      val Domain = Values("A","B")
      val x = Var("x",Domain)
      val y = Var("y",Domain)
      val sufficient = $(x === y) * unit("Equals")
      val w = VectorVar("w")
      val loglinear = LogLinear(sufficient,w,-Math.log(6.0))
      val incoming = new CompleteIgnorance[Any,EnvVar[Any]]
      val weights = new Vector
      weights.set(Math.log(2.0),"Equals")
      val result = loglinear.marginalizeLogLinear(incoming,weights)
      result.expectation(sufficient).get("Equals") must beCloseTo(2.0/3.0,0.00000001)
    }
  }

}
