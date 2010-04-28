package org.riedelcastro.thebeast

import env._
import java.util.Vector
import env.combinatorics.SpanningTreeConstraint
import env.vectors.VectorVar
import solve.SumProductBeliefPropagation


/**
 * @author Sebastian Riedel
 */

class DependencyParsingFixtures {
  import env.TheBeastImplicits._
  
  val maxLength = 5

  val Tokens = Ints(0 until maxLength)
  //todo: make this stateless!
  val Words = new MutableValues[String]()
  val Tags = new MutableValues[String]()
  val length = Var("length", Ints(1 until maxLength))
  val link = Predicate("link", Tokens x Tokens)
  val word = Predicate("word", Tokens x Words)
  val pos = Predicate("pos", Tokens x Tags)
  val token = Predicate("token", Tokens)


  def createSentence(words: List[String], tags: List[String], edges: Seq[(Int, Int)]) = {
    val sentence = new MutableEnv
    sentence(length) = words.size
    sentence.atoms(word) ++= words.zipWithIndex.map(_.swap)
    sentence.atoms(pos) ++= tags.zipWithIndex.map(_.swap)
    sentence.atoms(link) ++= edges
    sentence.atoms(token) ++= (0 until words.size)
    sentence.close(word, true)
    sentence.close(pos, true)
    sentence.close(link, true)
    sentence
  }

  def createTheManIsFast = {
    createSentence(
      List("Root", "The", "man", "is", "fast"),
      List("Root", "DT", "NN", "VB", "AD"),
      List((0, 3), (3, 2), (3, 4), (2, 1)))
  }

  def createTheMan = {
    createSentence(
      List("Root", "The", "man"),
      List("Root", "DT", "NN"),
      List((0, 2),(2, 1)))
  }

}

