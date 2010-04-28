package org.riedelcastro.thebeast.env.vectors

import org.riedelcastro.thebeast.env.TheBeastEnv
import org.specs.runner.{JUnit, JUnit4}
import org.specs.Specification


/**
 * @author Sebastian Riedel
 */


class VectorDotAppTest extends JUnit4(VectorDotAppSpecification)
object VectorDotAppSpecification extends Specification with TheBeastEnv with JUnit {
  "A Vector dot product application" should {
    "flatten its arguments when flattened" in {
      var dotproduct = (unit("A") + (unit("B") + unit("C") + unit("L"))) dot (unit("D") + unit("E"))
      var expected = (unit("A") + unit("B") + unit("C") + unit("L")) dot (unit("D") + unit("E"))
      dotproduct.flatten must_== expected
    }

  }
}