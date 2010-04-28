package org.riedelcastro.thebeast.env.combinatorics

import org.specs.Specification
import org.riedelcastro.thebeast.DependencyParsingFixtures
import org.specs.runner.{JUnit4}
import org.riedelcastro.thebeast.env._
import org.riedelcastro.thebeast.solve.ExhaustiveMarginalInference

/**
 * @author sriedel
 */

class SpanningTreeConstraintTest extends JUnit4(SpanningTreeConstraintSpecification)
object SpanningTreeConstraintSpecification extends Specification with TheBeastEnv {
  val trees = Seq(
    List((0, 1), (0, 2), (0, 3)),
    List((0, 1), (1, 2), (0, 3)),
    List((0, 1), (3, 2), (0, 3)),
    List((0, 1), (0, 2), (2, 3)),
    List((0, 1), (1, 2), (2, 3)),
    List((0, 1), (1, 3), (3, 2)),
    List((0, 1), (1, 2), (1, 3)),
    List((0, 2), (0, 3), (2, 1)),
    List((0, 2), (2, 1), (2, 3)),
    List((0, 3), (3, 1), (3, 2)),
    List((0, 3), (3, 1), (1, 2)),
    List((0, 3), (3, 2), (2, 1)))

  val counts = Map() ++ {for (i <- 0 until 4; j <- 1 until 4; if (j!=i)) yield
    (i,j)-> trees.filter(_.contains((i,j))).size.toDouble
  }

  "A projective spanning tree constraint" should {
    "return 1 if the the graph is a spanning tree" in {
      val fixtures = new DependencyParsingFixtures
      import fixtures._
      val sentence = createSentence(
        List("Root", "The", "man", "is", "fast"),
        List("Root", "DT", "NN", "VB", "AD"),
        List((0, 3), (3, 2), (3, 4), (2, 1)))
      val constraint = new SpanningTreeConstraint[Int](link, token, 0, LessThan(Tokens))
      sentence(constraint) must_== 1.0
    }
    "return 0 if the the graph has a cycle" in {
      val fixtures = new DependencyParsingFixtures
      import fixtures._
      val sentence = createSentence(
        List("root", "the", "man", "walks"),
        List("root", "DT", "NN", "VB"),
        List((0, 3), (1, 2), (2, 1)))
      val constraint = new SpanningTreeConstraint(link, token, 0, LessThan(Tokens))
      sentence(constraint) must_== 0.0
    }
    "return 0 if the the graph has a vertices with multiple parents" in {
      val fixtures = new DependencyParsingFixtures
      import fixtures._
      val sentence = createSentence(
        List("root", "the", "man", "walks"),
        List("root", "DT", "NN", "VB"),
        List((0, 3), (1, 2), (3, 2)))
      val constraint = new SpanningTreeConstraint(link, token, 0, LessThan(Tokens))
      sentence(constraint) must_== 0.0
    }
    "return 0 if the the graph is not spanning all vertices" in {
      val fixtures = new DependencyParsingFixtures
      import fixtures._
      val sentence = createSentence(
        List("root", "the", "man", "walks"),
        List("root", "DT", "NN", "VB"),
        List((0, 3), (3, 2)))
      val constraint = new SpanningTreeConstraint(link, token, 0, LessThan(Tokens))
      sentence(constraint) must_== 0.0
    }
    "return 0 if the the graph is not projective" in {
      val fixtures = new DependencyParsingFixtures
      import fixtures._
      def theManWalks(edges: List[(Int, Int)]) = createSentence(
        List("root", "the", "man", "walks"),
        List("root", "DT", "NN", "VB"), edges)

      val nonProjective = Seq(
        List((0, 2), (2, 3), (3, 1)),
        List((0, 2), (1, 3), (2, 1)))
      val constraint = new SpanningTreeConstraint(link, token, 0, LessThan(Tokens))
      nonProjective.map(theManWalks(_)).forall(sentence => sentence(constraint) must_== 0.0)
    }

    "return only edge variables that could be part of a spanning tree if root and vertices are grounded" in {
      val fixtures = new DependencyParsingFixtures
      import fixtures._
      val sentence = createTheMan
      val constraint = new SpanningTreeConstraint(link, token, 0, LessThan(Tokens))
      val expected = Set(FunAppVar(link, (0, 1)), FunAppVar(link, (0, 2)), FunAppVar(link, (1, 2)), FunAppVar(link, (2, 1)))
      val result = constraint.ground(sentence.mask(Set(link))).variables
      result must_== expected
    }
    "return exact marginals with exhaustive inference" in {
      val fixtures = new DependencyParsingFixtures
      import fixtures._
      val sentence = createSentence(
        List("root", "the", "man", "walks"),
        List("root", "DT", "NN", "VB"),
        List((0, 3), (3, 2), (2, 1)))
      val constraint = new SpanningTreeConstraint(link, token, 0, LessThan(Tokens))
      val grounded = constraint.ground(sentence.mask(Set(link)))
      val incoming = new CompleteIgnorance[Any, EnvVar[Any]]
      val exact = ExhaustiveMarginalInference.marginalize(grounded, incoming)
      for (edge <- counts.keySet) {
        exact.belief(FunAppVar(link,edge)).belief(true) must_== counts(edge)
      }
    }

    "return exact marginals with DP inference" in {
      val fixtures = new DependencyParsingFixtures
      import fixtures._
      val sentence = createSentence(
        List("root", "the", "man", "walks"),
        List("root", "DT", "NN", "VB"),
        List((0, 3), (3, 2), (2, 1)))
      val constraint = new SpanningTreeConstraint(link, token, 0, LessThan(Tokens))
      val grounded = constraint.ground(sentence.mask(Set(link)))
      val incoming = new CompleteIgnorance[Any, EnvVar[Any]]
      val result = grounded.marginalize(incoming)
      for (edge <- counts.keySet) {
        result.belief(FunAppVar(link,edge)).belief(true) must_== counts(edge)
      }

      
    }


  }

}


