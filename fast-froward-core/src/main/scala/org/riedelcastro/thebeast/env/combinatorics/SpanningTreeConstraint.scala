package org.riedelcastro.thebeast.env.combinatorics

import org.riedelcastro.thebeast.env._
import doubles.{DoubleConstant, DoubleTerm}
import collection.mutable.{HashSet, Stack, HashMap, MultiMap}
import java.lang.String
import tuples.TupleValues2

/**
 * A SpanningTreeConstraint is a term that maps graphs to 1 if they are
 * projective spanning trees over the set of vertices, and to 0 otherwise. Note
 * that for efficient processing vertices and root need to be ground
 * and edges needs to be a predicate.
 */
case class SpanningTreeConstraint[V](edges: Term[FunctionValue[(V, V), Boolean]],
                                     vertices: Term[FunctionValue[V, Boolean]],
                                     root: Term[V],
                                     order: Term[FunctionValue[(V, V), Boolean]]) extends DoubleTerm {
  def ground(env: Env): DoubleTerm = {
    SpanningTreeConstraint(edges.ground(env), vertices.ground(env), root.ground(env), order.ground(env))
  }

  def simplify: DoubleTerm = {
    val simplified = SpanningTreeConstraint(edges.simplify, vertices.simplify, root.simplify, order.simplify)
    val constant = simplified.eval(EmptyEnv)
    if (constant.isDefined) DoubleConstant(constant.get) else simplified
  }

  def asLogic: DoubleTerm = {
    import GenericImplicits._
    val domain = vertices.values.asInstanceOf[FunctionValues[V, Boolean]].domain
    val uniqueHead = forall(domain, domain, domain) {
      (h, i, o) => vertices(i) && vertices(h) && vertices(o) && edges(h, i) ~> !edges(o, i)
    }
    val dominates = Predicate("dominates", TupleValues2(domain, domain))
    val linkAndDominates = forall(domain, domain) {
      (h, i) => vertices(i) && vertices(h) && edges(h, i) ~> dominates(h, i)
    }
    val transitive = forall(domain, domain, domain) {
      (h, m, g) => vertices(h) && vertices(m) && vertices(g) && dominates(h, m) && edges(m, g) ~> dominates(h, g)
    }
    val acyclic = forall(domain) {
      i => vertices(i) ~> !dominates(i, i)
    }
    val projective1 = forall(domain, domain, domain) {
      (h, m, i) => vertices(h) && vertices(m) && vertices(i) && edges(h, m) && order(h, i) && order(i, m) ~> dominates(h, i)
    }
    val projective2 = forall(domain, domain, domain) {
      (h, m, i) => vertices(h) && vertices(m) && vertices(i) && edges(h, m) && order(m, i) && order(i, h) ~> dominates(h, i)
    }
    $ {uniqueHead && linkAndDominates && transitive && acyclic && projective1 && projective2}
  }


  def upperBound = 1.0

  def subterms = Seq(edges, vertices, root)

  def eval(env: Env): Option[Double] = {
    //get edges map
    val v = Set() ++ env(vertices).getSources(Some(true))
    val e = env(this.edges).getSources(Some(true)).filter(edge => v(edge._1) && v(edge._2))
    val r = env(root)
    val heads = new HashMap[V, V]
    //check if each vertex has at most one parent
    for (edge <- e) {
      if (heads.contains(edge._2)) return Some(0.0)
      heads(edge._2) = edge._1
    }
    //check if each vertex has at least one parent, unless it's the root
    if (v.exists(vertex => vertex != r && !heads.isDefinedAt(vertex))) return Some(0.0)
    val indices = new HashMap[V, Int]
    val lowlinks = new HashMap[V, Int]
    val stack = new Stack[V]
    val roots = new HashSet[V]
    var index = 0
    //check for cycles
    for (vertex <- v) {
      if (!indices.isDefinedAt(vertex)) tarjan(vertex)
      if (!roots.isEmpty) return Some(0.0)
    }
    def tarjan(vertex: V) {
      indices(vertex) = index
      lowlinks(vertex) = index
      index += 1
      stack.push(vertex)
      for (head <- heads.get(vertex)) {
        if (!indices.isDefinedAt(head)) {
          tarjan(head)
          lowlinks(vertex) = Math.min(lowlinks(vertex), lowlinks(head))
        } else if (stack.contains(head)) {
          lowlinks(vertex) = Math.min(lowlinks(vertex), indices(head))
        }
      }
      if (lowlinks(vertex) == indices(vertex)) {
        if (stack.top != vertex) roots += vertex
        var top = vertex
        do {
          top = stack.pop
        } while (top != vertex)

      }
    }
    //test projectiveness
    val lessThan = EmptyEnv(this.order)

    //sort vertices according to order
    val sorted = v.toList.sort((x, y) => x == root || lessThan(x, y)).toArray
    val n = sorted.size
    val vertex2index = Map() ++ (for (i <- 0 until n) yield sorted(i) -> i)
    //mapping from vertex to children
    val edges = for (i <- 1 until n) yield (vertex2index(heads(sorted(i))), i)

    def cross(e1: (Int, Int), e2: (Int, Int)): Boolean = {
      val e1l = Math.min(e1._1, e1._2)
      val e1r = Math.max(e1._1, e1._2)
      val e2l = Math.min(e2._1, e2._2)
      val e2r = Math.max(e2._1, e2._2)
      !(e1l >= e2l && e1r <= e2r || e2l >= e1l && e2r <= e1r || e2r <= e1l || e1r <= e2l)
    }
    //todo this should be doable in O(n)
    for (e1 <- edges; e2 <- edges; if (e1 != e2)) {
      if (cross(e1, e2)) return Some(0.0)
    }
    Some(1.0)
  }


  def values = Values(0.0, 1.0)

  def variables = {
    if (vertices.isGround && root.isGround && order.isGround && edges.isInstanceOf[Predicate[_]]) {
      linkVariables.asInstanceOf[Set[EnvVar[Any]]]
    } else
      edges.variables ++ vertices.variables ++ root.variables ++ order.variables
  }

  private def linkVariables: scala.collection.immutable.Set[FunAppVar[(V, V), Boolean]] = {
    val pred = edges.asInstanceOf[Predicate[(V, V)]]
    val v = EmptyEnv(vertices).getSources(Some(true))
    val r = EmptyEnv(root)
    Set() ++ (for (source <- v; dest <- v; if (dest != r && dest != source))
    yield FunAppVar(pred, (source, dest)))
  }


  override def marginalize(incoming: Beliefs[Any, EnvVar[Any]]): Beliefs[Any, EnvVar[Any]] = {
    if (vertices.isGround && root.isGround && order.isGround && edges.isInstanceOf[Predicate[_]]) {

      val pred = edges.asInstanceOf[Predicate[(V, V)]]
      val vertices = EmptyEnv(this.vertices).getSources(Some(true))
      val root = EmptyEnv(this.root)
      val lessThan = EmptyEnv(this.order)

      //sort vertices according to order
      val sorted = vertices.toList.sort((x, y) => x == root || lessThan(x, y)).toArray

      //a la Smith and Eisner 2008
      //weights are by default 0
      val weights = new HashMap[(Int, Int), Double] {
        override def default(p: (Int, Int)) = 0.0
      }
      var pi = 1.0
      //calculate weights and pi
      for (i <- 0 until sorted.size; j <- 1 until sorted.size; if (i != j)) {
        val belief = incoming.belief(FunAppVar(pred, (sorted(i), sorted(j))))
        weights(i -> j) = belief.belief(true) / belief.belief(false)
        pi *= belief.belief(false)
      }
      //calculate total weights of all trees with a given edge, and partitition function
      val insideOutside = InsideOutsideAlgorithm.calculate(sorted, weights)
      //partition function a la S&E 08
      val b = insideOutside.Z * pi

      //calculate beliefs for true and false states
      val beliefs = new MutableBeliefs[Any, EnvVar[Any]]
      for (i <- 0 until sorted.size; j <- 1 until sorted.size; if (i != j)) {
        val atom = FunAppVar(pred, (sorted(i), sorted(j)))
        val trueBelief = insideOutside.total(i, j) * pi
        beliefs.increaseBelief(atom, true, trueBelief)
        beliefs.increaseBelief(atom, false, b - trueBelief)
      }
      beliefs
    } else
      super.marginalize(incoming)
  }


  object InsideOutsideAlgorithm {
    object SpanType extends Enumeration {
      type SpanType = Value
      val RightParent, LeftParent, NoParents = Value

      val parents = Seq(LeftParent, RightParent)

      def opposite(value: Value) = value match {
        case LeftParent => RightParent
        case RightParent => LeftParent
        case _ => NoParents
      }

      def toType(left: Boolean, right: Boolean): Value = {
        if (left && !right) LeftParent
        else if (!left && right) RightParent
        else if (!left && !right) NoParents
        else null
      }

    }
    import SpanType._



    case class Signature(from: Int, to: Int, rightWard: Boolean, link: Boolean)


    class InsideOutsideResult {
      val inside = new HashMap[Signature, Double]
      val outside = new HashMap[Signature, Double]
      val total = new HashMap[(Int, Int), Double]
      var Z = 0.0

      def in(from: Int, to: Int, rightWard: Boolean, link: Boolean): Double = {
        inside.getOrElse(Signature(from, to, rightWard, link), 0.0)
      }

      def incrIn(from: Int, to: Int, rightWard: Boolean, link: Boolean, value: Double) = {
        val sig = Signature(from, to, rightWard, link)
        inside(sig) = inside.getOrElse(sig, 0.0) + value
      }

      def out(from: Int, to: Int, rightWard: Boolean, link: Boolean): Double = {
        outside.getOrElse(Signature(from, to, rightWard, link), 0.0)
      }

      def incrOut(from: Int, to: Int, rightWard: Boolean, link: Boolean, value: Double) = {
        val sig = Signature(from, to, rightWard, link)
        outside(sig) = outside.getOrElse(sig, 0.0) + value
      }

    }

    def calculate(sorted: Array[V], weights: scala.collection.Map[(Int, Int), Double]): InsideOutsideResult = {

      /**
       * Calculated according to "inside and outside probs using "Reestimation and Best-First Parsing
       * Algorithm for Probabilistic Dependency Grammars" by Lee and Choi.
       */

      val bools = Array(false, true)
      val result = new InsideOutsideResult
      import result._

      val n = sorted.size

      //init unit values
      for (i <- 0 until n - 1) {
        incrIn(i, i + 1, true, true, weights(i, i + 1))
        incrIn(i, i + 1, true, false, weights(i, i + 1))
        incrIn(i, i + 1, false, true, weights(i + 1, i))
        incrIn(i, i + 1, false, false, weights(i + 1, i))
      }
      for (i <- 0 until n) {
        incrIn(i, i, true, false, 1.0)
        incrIn(i, i, false, false, 1.0)
      }

      for (width <- 2 until n) {
        for (i <- 0 until n - width) {
          val j = i + width
          //complete link inside
          for (m <- i until j) {
            incrIn(i, j, true, true, in(i, m, true, false) * in(m + 1, j, false, false) * weights(i, j))
            incrIn(i, j, false, true, in(i, m, true, false) * in(m + 1, j, false, false) * weights(j, i))
          }
          //complete sequence inside
          for (m <- i until j) {
            incrIn(i, j, true, false, in(i, m, true, false) * in(m, j, true, true))
          }
          for (m <- i + 1 until j + 1) {
            incrIn(i, j, false, false, in(i, m, false, true) * in(m, j, false, false))
          }
        }
      }

      Z = in(0, n - 1, true, false)

      //outside probabilities
      incrOut(0, n - 1, true, true, 1.0)
      incrOut(0, n - 1, true, false, 1.0)

      for (width <- (1 until n - 1).reverse) {
        for (i <- 0 until n - width) {
          val j = i + width
          //complete sequence outside
          for (h <- j + 1 until n) {
            incrOut(i, j, true, false, out(i, h, true, false) * in(j, h, true, true) +
                    out(i, h, true, true) * in(j + 1, h, false, false) * weights(i, h) +
                    out(i, h, false, true) * in(j + 1, h, false, false) * weights(h, i))
          }
          for (v <- 0 until i) {
            incrOut(i, j, false, false, out(v, j, false, false) * in(v, i, false, true) +
                    out(v, j, true, true) * in(v, i - 1, true, false) * weights(v, j) +
                    out(v, j, false, true) * in(v, i - 1, true, false) * weights(j, v))
          }

          //complete link outside
          for (v <- 0 until i + 1) {
            incrOut(i, j, true, true, out(v, j, true, false) * in(v, i, true, false))
          }
          for (h <- j until n) {
            incrOut(i, j, false, true, out(i, h, false, false) * in(j, h, false, false))
          }


        }
      }


      for (i <- 0 until n; j <- i + 1 until n) {
        total(i -> j) = in(i, j, true, true) * out(i, j, true, true)
        total(j -> i) = in(i, j, false, true) * out(i, j, false, true)
      }

      result
    }

  }

}

