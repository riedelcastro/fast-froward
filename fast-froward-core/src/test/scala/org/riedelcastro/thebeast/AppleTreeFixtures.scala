package org.riedelcastro.thebeast


import env.doubles._

/**
 * @author Sebastian Riedel
 */

trait AppleTreeFixtures {
  import env.TheBeastImplicits._

  val Sick = "Sick" <~ Bools
  val Dry = "Dry" <~ Bools
  val Loses = "Loses" <~ Bools
  val SickParameters = PriorPDParams(Bools, (true -> 0.1))
  val DryParameters = PriorPDParams(Bools, (true -> 0.1))
  val LosesParameters = CPDParams(Bools, Bools x Bools,
    (true, (false, false)) -> 0.02, (true, (false, true)) -> 0.85,
    (true, (true, false)) -> 0.90, (true, (true, true)) -> 0.95)
  val appleTreeModel = Multiplication(Seq(
    PriorPD(Sick, SickParameters),
    PriorPD(Dry, DryParameters),
    CPD(Loses ||| (Sick, Dry), LosesParameters)
    ))

  val beliefForSick = 0.1
  val beliefForLoses = 0.1832
}