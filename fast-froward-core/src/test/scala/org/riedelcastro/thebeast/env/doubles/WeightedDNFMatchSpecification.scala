package org.riedelcastro.thebeast.env.doubles

import org.specs.Specification
import org.riedelcastro.thebeast.env.booleans.BooleanVar
import org.riedelcastro.thebeast.env.{TheBeastEnv, Ints, Predicate, TheBeastImplicits}
import org.specs.runner.JUnit4

/**
 * Created by IntelliJ IDEA.
 * User: riedelcastro
 * Date: Oct 31, 2009
 * Time: 9:49:23 PM
 * To change this template use File | Settings | File Templates.
 */
class WeightedDNFMatchTest extends JUnit4(WeightedDNFMatchSpecification)
object WeightedDNFMatchSpecification extends Specification{
  import TheBeastImplicits._

  "WeightedDNFMatch" should {
    "match a weighted DNF formula with function application variables with modified arguments" in {
      val Numbers = Ints(0 until 10)
      val pred = Predicate("node", Numbers)
      val wdnf = $(pred(0) <~> pred(0 + 1)) * Uniform(-2.0, 2.0)
      WeightedDNFMatch.unapply(wdnf) must beSomething
    }


  }
}