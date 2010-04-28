package org.riedelcastro.thebeast.env.doubles

import org.riedelcastro.thebeast.env._
import booleans._
import collection.mutable.ArrayBuffer
import org.riedelcastro.thebeast.util.Trackable

/**
 * Created by IntelliJ IDEA.
 * User: riedelcastro
 * Date: Oct 29, 2009
 * Time: 10:16:53 PM
 * To change this template use File | Settings | File Templates.
 */

case class WeightedDNF(val dnf: DNF[BooleanLiteral], weight: Double)
    extends Multiplication(Seq(Indicator(dnf), DoubleConstant(weight))) {

  val TableRepresentation = new TableRepresentation(dnf)

  override def variables = Set() ++ (for (con <- dnf.args; lit <- con.args) yield lit.variable)

  override def marginalize(incoming: Beliefs[Any, EnvVar[Any]]) = {
    TableRepresentation.marginalize(incoming, weight, false)
  }
}

case class ExpWeightedDNF(val dnf: DNF[BooleanLiteral], weight: Double)
    extends Exp(Multiplication(Seq(Indicator(dnf), DoubleConstant(weight)))) {

  val TableRepresentation = new TableRepresentation(dnf)

  override def variables = Set() ++ (for (con <- dnf.args; lit <- con.args) yield lit.variable)

  override def marginalize(incoming: Beliefs[Any, EnvVar[Any]]) = {
    TableRepresentation.marginalize(incoming, Math.exp(weight) - 1.0, true)
  }
}

//todo:  should this be refactored/moved to booleans? Do we need a general Table framework for CNFs etc.?
class TableRepresentation(val dnf:DNF[BooleanLiteral]) extends Trackable {
  val vars = (Set() ++ (for (con <- dnf.args; lit <- con.args) yield lit.variable)).toArray
  val rows = new ArrayBuffer[Array[Boolean]]
  for (con <- dnf.args) {
    val fixedPart = new Array[Boolean](vars.size)
    val argIndices = con.args.map(l=>vars.indexOf(l.variable))
    val noArgIndices = vars.filter(!con.args.map(_.variable).contains(_)).map(vars.indexOf(_))
    for (arg <- con.args) fixedPart(vars.indexOf(arg.variable)) = !arg.negated
    val numberOfNoArgs = vars.size - con.args.size
    val openPart = new Array[Boolean](numberOfNoArgs)
    for (assignmentId <- 0 until Math.pow(2,numberOfNoArgs).toInt){
      //generate/set the states of the open variables
      for (i <- 0 until numberOfNoArgs)
        if (assignmentId % Math.pow(2,i).toInt == 0) openPart(i) = ! openPart(i)
      val row = new Array[Boolean](vars.size)
      fixedPart.copyToArray(row,0)
      for (i <- 0 until numberOfNoArgs) //noArgIndex <- noArgIndices)
        row(noArgIndices(i)) = openPart(i)
      if (!rows.exists(_.deepEquals(row)))
        rows += row
    }
  }

  lazy val domainSize = Math.pow(2,vars.size).toInt

  def marginalize(incoming: Beliefs[Any, EnvVar[Any]], rowScore:Double, offSet:Boolean) = {
    |**("Marginalize Table " + rows.map(_.mkString(" ")).mkString("(",",",")"))
    val incomingBoolVars = incoming.asInstanceOf[Beliefs[Boolean, EnvVar[Boolean]]]
    val result = new MutableBeliefs[Boolean, EnvVar[Boolean]]
    //iterate over conjunctions
    for (row <- rows) {
      var score = rowScore
      for (argIndex <- 0 until vars.size)
        score *= incoming.belief(vars(argIndex)).belief(row(argIndex))
      for (argIndex <- 0 until vars.size)
        result.increaseBelief(vars(argIndex),row(argIndex), score)
    }
    if (offSet) {
      val partitionFunction = incoming.partitionFunction
      for (v <- vars){
        val belief = incoming.belief(v)
        val factor = partitionFunction / belief.totalSum
        result.increaseBelief(v,true, factor * belief.belief(true))
        result.increaseBelief(v,false, factor * belief.belief(false))

      }
    }
    **|
    result.asInstanceOf[Beliefs[Any, EnvVar[Any]]]
  }

}


object WeightedDNFMatch {
  def unapply(term:DoubleTerm):Option[WeightedDNF] = {
    term match {
      case Multiplication(Seq(Indicator(LiteralDNFMatch(dnf)),Grounded(w))) => Some(WeightedDNF(dnf,w.asInstanceOf[Double]))
      case Multiplication(Seq(Grounded(w),Indicator(LiteralDNFMatch(dnf)))) => Some(WeightedDNF(dnf,w.asInstanceOf[Double]))
      case _=> None
    }
  }
}

object ExpWeightedDNFMatch {
  def unapply(term:DoubleTerm):Option[ExpWeightedDNF] = {
    term match {
      case Exp(WeightedDNFMatch(weightedDNF)) => Some(ExpWeightedDNF(weightedDNF.dnf, weightedDNF.weight))
      case _ => None
    }
  }
}