package org.riedelcastro.thebeast.env.doubles

import org.specs.Specification
import org.riedelcastro.thebeast.env.{EnvVar, MutableBeliefs, TheBeastImplicits}
import org.riedelcastro.thebeast.env.booleans._
import org.riedelcastro.thebeast.solve.ExhaustiveMarginalInference
import org.specs.runner.JUnit4

/**
 * Created by IntelliJ IDEA.
 * User: riedelcastro
 * Date: Oct 31, 2009
 * Time: 1:19:28 AM
 * To change this template use File | Settings | File Templates.
 */
class WeightedDNFTest extends JUnit4(WeightedDNFSpecification)
object WeightedDNFSpecification extends Specification {
  import TheBeastImplicits._

  "A Weighted DNF" should {
    val A = BooleanVar("A")
    val B = BooleanVar("B")
    val C = BooleanVar("C")
    val incomingBeliefs = new MutableBeliefs[Any,EnvVar[Any]]
    incomingBeliefs.increaseBelief(A,true,1.0)
    incomingBeliefs.increaseBelief(A,false,2.0)
    incomingBeliefs.increaseBelief(B,true,1.0)
    incomingBeliefs.increaseBelief(B,false,1.0)
    incomingBeliefs.increaseBelief(C,true,1.0)
    incomingBeliefs.increaseBelief(C,false,3.0)

    "calculate exact marginals" in {
      val weightedDnf = new WeightedDNF(DNF
            (Seq(Conjunction(Seq(NegatedVar(A),B)), Conjunction(Seq(B,C)))),1.0)
      val result = weightedDnf.marginalize(incomingBeliefs)
      val expected = ExhaustiveMarginalInference.marginalize(weightedDnf,incomingBeliefs)
      result must_== expected
    }
    "calculate exact marginals if argument to exp function" in {
      val expWeightedDnf = new ExpWeightedDNF(DNF
            (Seq(Conjunction(Seq(NegatedVar(A),B)), Conjunction(Seq(B,C)))),2.0)
      val result = expWeightedDnf.marginalize(incomingBeliefs)
      val expected = ExhaustiveMarginalInference.marginalize(expWeightedDnf,incomingBeliefs)
      result must_== expected
    }
  }
}