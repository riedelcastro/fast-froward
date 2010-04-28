package org.riedelcastro.thebeast.env.doubles

import org.riedelcastro.thebeast.env.booleans.{NotApp, Disjunction}
import org.specs.runner.JUnit4
import org.specs.Specification
import org.riedelcastro.thebeast.env.TheBeastEnv
import org.riedelcastro.thebeast.CitationMatchingFixtures

/**
 * @author Sebastian Riedel
 */
class AlchemyIndicatorTest extends JUnit4(AlchemyIndicatorSpecification)
object AlchemyIndicatorSpecification extends Specification with TheBeastEnv with CitationMatchingFixtures {
  "An AlchemyIndicator" should {
    "be a normalized sum of standard indicator functions applied to the disjunctions in a CNF " +
            "representation of the original formula" in {
      val indicator = $$((same("A","B") && same("B","C")) ~> same("A","C"))
      val expected = Sum(Seq(Indicator(
        Disjunction(Seq(NotApp(same("A","B")),NotApp(same("B","C")),same("A","C")))) * 1.0))
      indicator.asInstanceOf[Sum[DoubleTerm]] must_== indicator

    }
  }

}