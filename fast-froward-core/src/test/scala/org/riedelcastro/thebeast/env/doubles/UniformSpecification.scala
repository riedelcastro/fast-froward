package org.riedelcastro.thebeast.env.doubles


import booleans.{NotApp, Disjunction}
import specs.runner.JUnit4
import specs.Specification

/**
 * @author Sebastian Riedel
 */


class UniformTest extends JUnit4(UniformSpecification)
object UniformSpecification extends Specification with TheBeastEnv {
  "An Uniform Term" should {
    "evaluate to the same random number if evaluated twice with the same " +
            "interval arguments" in {
      val from = DoubleVar("from", Values(0.0,2.0,4.0,8.0))
      val to = DoubleVar("to", Values(0.0,2.0,4.0,8.0))
      val term = Uniform(from,to)
      val binding = new MutableEnv
      binding.mapTo(from)->0.0
      binding.mapTo(to)->2.0
      binding(term) must_== binding(term)
    }
    "evaluate to different random numbers if evaluated twice with different " +
            "non-overlapping interval arguments" in {
      val from = DoubleVar("from", Values(0.0,2.0,4.0,8.0))
      val to = DoubleVar("to", Values(0.0,2.0,4.0,8.0))
      val term = Uniform(from,to)
      val binding1 = new MutableEnv
      binding1.mapTo(from)->0.0
      binding1.mapTo(to)->2.0
      val binding2 = new MutableEnv
      binding2.mapTo(from)->4.0
      binding2.mapTo(to)->8.0
      binding1(term) must_!= binding2(term)
    }
    "be grounded to independent Uniform terms that (most likely) evaluate to different random values " in {
      val term = Uniform(0.0,1.0)
      val binding = new MutableEnv
      binding(term) must_!= binding(term.ground(binding))
      binding(term.ground(binding)) must_!= binding(term.ground(binding)) 

    }
  }

}