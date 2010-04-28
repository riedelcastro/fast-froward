package org.riedelcastro.thebeast.env.vectors


import org.specs.runner.{JUnit, JUnit4}
import org.riedelcastro.thebeast.env.TheBeastEnv
import org.specs.Specification

/**
 * @author Sebastian Riedel
 */

class VectorTest extends JUnit4(VectorSpecification)
object VectorSpecification extends Specification with TheBeastEnv with JUnit {
  "A Vector" should {
    "add values in place" in {
      var x = new Vector
      x.set(1.0,1)
      var y = new Vector
      y.set(1.0,1)
      x.addInPlace(y,1.0)
      x.get(1) must_== 2.0
    }
    "calculate dot product" in {
      var x = new Vector
      x.set(2.0,1)
      var y = new Vector
      y.set(2.0,1)
      x dot y must_== 4.0
    }
    "store values" in {
      var x = new Vector
      x.set(1.0,1)
      x.get(1) must_== 1.0
    }
    "store values for multi-dimensional indices" in {
      var x = new Vector
      x.set(1.0,1,"A")
      x.get(1,"A") must_== 1.0
    }
    "set default values depending on first key" in {
      var x = new Vector
      x.set(1.0,1,"A")
      x.setDefaultForFirstKey(1,100.0)
      x.get(1,"C") must_== 100.0
    }

  }
}