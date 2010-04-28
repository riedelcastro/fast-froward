package org.riedelcastro.thebeast.env.doubles

import org.riedelcastro.thebeast.env.vectors.{VectorDotApp, VectorTerm, VectorSum, QuantifiedVectorSum}
import org.riedelcastro.thebeast.env.booleans._
import org.riedelcastro.thebeast.util.Logging

/**
 * @author sriedel
 */

object Factorizer extends Logging {
  def toMultiplication(term: DoubleTerm): Multiplication[DoubleTerm] = {
    debug("Factorizing %s".format(term))
    unroll(term).flatten match {
      case Multiplication(args) => Multiplication(args.flatMap(toMultiplication(_).args))
      case Exp(Sum(args)) => Multiplication(args.map(Exp(_)))
      case Exp(v: VectorDotApp) => Multiplication(v.distribute.asInstanceOf[Sum[DoubleTerm]].args.map(Exp(_)))
      case Indicator(b) => Multiplication(factorize(b).args.map(Indicator(_)))
      case x => Multiplication(Seq(x))
    }
  }

  def factorize(term:BooleanTerm) : Conjunction[BooleanTerm] = {
    term.flatten match {
      case Conjunction(args) => Conjunction(args.flatMap(factorize(_).args))
      case x => Conjunction(Seq(x))
    }
  }

  //todo: this should all be done by a generic unroll method and the generic builder framework
  private def unroll(term: DoubleTerm): DoubleTerm = {
    debug("Unrolling %s".format(term))
    term match {
      case Multiplication(args) => Multiplication(args.map(unroll(_)))
      case Sum(args) => Sum(args.map(unroll(_)))
      case x: QuantifiedSum[_] => x.unroll
      case Normalize(x) => Normalize(unroll(x))
      case Exp(x) => Exp(unroll(x))
      case Indicator(arg) => Indicator(unrollBoolean(arg))
      case VectorDotApp(lhs, rhs) => VectorDotApp(unrollVectorTerm(lhs), unrollVectorTerm(rhs))
      case x => x
    }
  }

  private def unrollBoolean(term: BooleanTerm): BooleanTerm = {
    debug("Unrolling %s".format(term))
    term match {
      case Conjunction(args) => Conjunction(args.map(unrollBoolean(_)))
      case Disjunction(args) => Disjunction(args.map(unrollBoolean(_)))
      case Forall(v, f) => Forall(v, unrollBoolean(f)).unrollUncertain
      case x => x
    }
  }

  private def unrollVectorTerm(term: VectorTerm): VectorTerm = term match {
    case VectorSum(args) => VectorSum(args.map(unrollVectorTerm(_)))
    case QuantifiedVectorSum(v, f) => {
      val x = QuantifiedVectorSum(v, unrollVectorTerm(f)).unrollUncertain
      x
    }
    case x => x
  }

}