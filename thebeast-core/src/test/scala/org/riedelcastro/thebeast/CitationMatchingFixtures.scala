package org.riedelcastro.thebeast


import env.{Env, MutableEnv, Values}

/**
 * @author Sebastian Riedel
 */

trait CitationMatchingFixtures {
  import env.TheBeastImplicits._

  val Citations = Values("A", "B", "C")
  val same = "same" <~ (Citations x Citations) -> Bools
  val similar = "similar" <~ (Citations x Citations) -> Bools

  def createWorldWhereABAreSimilarAndSame:Env = {
    var y1 = new MutableEnv
    y1.close(same, true)
    y1.close(similar, true)
    y1.mapTo(same)(("A", "B")) -> true
    y1.mapTo(similar)(("A", "B")) -> true
    y1
  }

  def createWorldWhereABAreSimilarButABCAreSame:Env = {
    var y2 = new MutableEnv
    y2.close(same, true)
    y2.close(similar, true)
    y2.mapTo(same)(("A", "B")) -> true
    y2.mapTo(same)(("B", "C")) -> true
    y2.mapTo(same)(("A", "C")) -> true
    y2.mapTo(similar)(("A", "B")) -> true
    y2.mapTo(similar)(("B", "C")) -> true
    y2
  }


}