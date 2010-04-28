package org.riedelcastro.thebeast.env.vectors

import org.specs.Specification
import org.specs.runner.{JUnit4, JUnit}
import org.riedelcastro.thebeast.env.{MutableEnv, Predicate, Ints, TheBeastEnv}
import org.riedelcastro.thebeast.env.ints.IntConstant

/**
 * @author sriedel
 */

class QuantifiedVectorSumTest extends JUnit4(QuantifiedVectorSumSpecification)
object QuantifiedVectorSumSpecification extends Specification with TheBeastEnv with JUnit {
   "A quantified vector sum" should {
    "only unroll the terms which cannot already be evaluated to values/constants when " +
            "the unrollUncertain method is called" in {

      val ints = Ints(1 until 5)
      val observed = Predicate("obs",ints)
      val hidden = Predicate("hidden",ints)
      val world = new MutableEnv
      world.atoms(observed) ++= List(2,3)
      world.close(observed, true)
      val formula = vectorSum(ints){i=> $(observed(i) && hidden(i)) * unit(i)}

      val unrolled = formula.ground(world).unrollUncertain

      unrolled.args.size must_== 2
      unrolled.args(0) must_== ($(observed(2) && hidden(2)) * unit(2)).ground(world)
      unrolled.args(1) must_== ($(observed(3) && hidden(3)) * unit(3)).ground(world)
    }

  }



}
