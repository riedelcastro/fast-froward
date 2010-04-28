package org.riedelcastro.thebeast.env.doubles

import org.riedelcastro.thebeast.env.booleans.{LiteralMatch, BooleanLiteral}
import org.riedelcastro.thebeast.env._
import org.riedelcastro.thebeast.util.Trackable

/**
 * Created by IntelliJ IDEA.
 * User: riedelcastro
 * Date: Nov 2, 2009
 * Time: 12:17:33 AM
 * To change this template use File | Settings | File Templates.
 */

case class WeightedBooleanLiteral(literal:BooleanLiteral, weight:Double)
  extends Multiplication(Seq(Indicator(literal),DoubleConstant(weight))) {
  //todo: do we need marginalization for the non exp version? probably not
  override def marginalize(incoming: Beliefs[Any, EnvVar[Any]]) = {
    val result = new MutableBeliefs[Any,EnvVar[Any]]
    result.increaseBelief(literal.variable, !literal.negated,
                         incoming.belief(literal.variable).belief(!literal.negated) * weight)
    result
  }
}

case class ExpWeightedBooleanLiteral(literal:BooleanLiteral, weight:Double)
  extends Exp(Multiplication(Seq(Indicator(literal),DoubleConstant(weight)))) with Trackable{

  val (trueScore,falseScore) = if (literal.negated) (1.0,Math.exp(weight)) else (Math.exp(weight), 1.0)

  override def marginalize(incoming: Beliefs[Any, EnvVar[Any]]) = {
    |**("Calculate marginals for exp weighted boolean literal")
    val result = new MutableBeliefs[Any,EnvVar[Any]]
    result.increaseBelief(literal.variable, true,incoming.belief(literal.variable).belief(true) * trueScore)
    result.increaseBelief(literal.variable, false, incoming.belief(literal.variable).belief(false) * falseScore)
    **|
    result
  }

}

object WeightedBooleanLiteralMatch {
  def unapply(term:Term[Double]) : Option[WeightedBooleanLiteral] = term match {
    case Multiplication(Seq(Indicator(LiteralMatch(lit)),Grounded(w))) => Some(WeightedBooleanLiteral(lit,w.asInstanceOf[Double]))
    case Multiplication(Seq(Grounded(w),Indicator(LiteralMatch(lit)))) => Some(WeightedBooleanLiteral(lit,w.asInstanceOf[Double]))
    case _ => None
  }
}

object ExpWeightedBooleanLiteralMatch {
  def unapply(term:Term[Double]) : Option[ExpWeightedBooleanLiteral] = term match {
    case Exp(WeightedBooleanLiteralMatch(wlit)) => Some(ExpWeightedBooleanLiteral(wlit.literal,wlit.weight))
    case _ => None
  }
}