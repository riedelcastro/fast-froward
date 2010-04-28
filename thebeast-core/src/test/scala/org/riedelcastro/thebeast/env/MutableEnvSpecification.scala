package org.riedelcastro.thebeast.env


import specs.runner.{JUnit4, JUnit}
import specs.Specification

/**
 * @author Sebastian Riedel
 */

class MutableEnvTest extends JUnit4(MutableEnvSpecification)
object MutableEnvSpecification extends Specification with TheBeastEnv with JUnit {
  "A Mutable Environment" should {
    "set atomic variables to values" in {
      var x = "x" <~ Values(1,2,3)
      var env = new MutableEnv
      env += x -> 1
      env(x) must_== 1
    }
    "set partial function mappings" in {
      var f = "f" <~ Values(1,2,3) -> (Values("A","B") -> Values(false,true))
      var env = new MutableEnv
      env.mapTo(f)(1)("A") ->true
      env(f(1)("A")) must_== true
    }
    "return a default value for unset closed variables" in {
      var f = "f" <~ Values(1,2,3) -> (Values("A","B") -> Values(false,true))
      var env = new MutableEnv
      env.close(f,true)
      env.mapTo(f)(1)("A") ->true
      env(f(1)("B")) must_== false
    }
    "mask an already set variable" in {
      var f = "f" <~ Values(1,2,3) -> (Values("A","B") -> Values(false,true))
      var env = new MutableEnv
      env.mapTo(f)(1)("A") ->true
      env.mask(Set(f)).eval(f(1)("A")) must_== None
    }
    "return None for unset variables" in {
      var f = "f" <~ Values(1,2,3) -> Values(false,true)
      var env = new MutableEnv
      env.eval(f(1)) must_== None      
    }
  }

}