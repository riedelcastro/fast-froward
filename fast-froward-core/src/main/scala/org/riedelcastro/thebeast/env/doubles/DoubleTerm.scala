package org.riedelcastro.thebeast.env.doubles

import org.riedelcastro.thebeast._
import env._
import booleans.BooleanTerm
import collection.mutable.{HashSet, HashMap}
import functions._
import solve.ExhaustiveMarginalInference
import tuples.TupleTerm2
import vectors.{VectorTerm, VectorScalarApp}

/**
 * @author Sebastian Riedel
 */

trait DoubleTerm extends BoundedTerm[Double] {
  def +(rhs: DoubleTerm) = AddApp(this, rhs)

  def *(rhs: DoubleTerm) = TimesApp(this, rhs)

  def *(rhs: VectorTerm) = VectorScalarApp(rhs, this)

  def marginalize(incoming: Beliefs[Any,EnvVar[Any]]): Beliefs[Any,EnvVar[Any]] = {
    ExhaustiveMarginalInference.marginalize(this,incoming)
  }

  //def ground(env:Env) : DoubleTerm = super.ground(env).asInstanceOf[DoubleTerm]

  def simplify: DoubleTerm

  def ground(env: Env): DoubleTerm

  def flatten: DoubleTerm = this

  def apply(variables:EnvVar[_]*) = {
    Objective(Set() ++ variables,this)
  }

  def ?(variables:EnvVar[_]*) = {
    Objective(Set() ++ variables,this)
  }

}



case class DoubleVar(override val name: String, override val values: Values[Double])
        extends Var[Double](name, values) with DoubleTerm {
  def upperBound = Math.POS_INF_DOUBLE

  override def ground(env: Env) = env.eval(this).map(new DoubleConstant(_) with GroundedConstant).getOrElse(this)


  override def simplify = this
}

case class CPDParams[O, C](outcomeValues: Values[O], conditionValues: Values[C], mapping: Pair[(O, C), Double]*)
        extends HashMap[(O, C), Double] {
  for (pair <- mapping) this += pair

  for (condition <- conditionValues) {
    val undefined = new HashSet[O]
    var totalProb = 0.0
    for (outcome <- outcomeValues) {
      this.get((outcome, condition)) match {
        case Some(prob) => totalProb += prob
        case None => undefined += outcome
      }
    }
    val probOfRest = (1.0 - totalProb) / undefined.size
    for (outcome <- undefined) {
      this += (outcome, condition) -> probOfRest
    }
  }
}



case class PriorPDParams[O](override val outcomeValues: Values[O], singleMapping: Pair[O, Double]*)
        extends CPDParams(outcomeValues, Singleton, singleMapping.map(pair => ((pair._1, Singleton), pair._2)): _*)

case class CPD[O, C](conditioned: ConditionedTerm[O, C], parameters: Term[Tuple2[O, C] => Double])
        extends DoubleFunApp(parameters, TupleTerm2(conditioned.term, conditioned.condition)) {
}

case class PriorPD[O](outcome: Term[O], override val parameters: Term[Tuple2[O, SingletonClass] => Double])
        extends CPD(ConditionedTerm(outcome, Singleton), parameters) {
}


case class Exp(override val arg: DoubleTerm) extends DoubleFunApp(Constant(ExpOp), arg) {
  override def ground(env: Env) = Exp(arg.ground(env))

  override def flatten = Exp(arg.flatten)
}


case class AddApp(lhs: DoubleTerm, rhs: DoubleTerm) extends Sum(Seq(lhs, rhs))

case class TimesApp(lhs: DoubleTerm, rhs: DoubleTerm) extends Multiplication(Seq(lhs, rhs))

case class Sum[+T <: DoubleTerm](override val args: Seq[T]) extends Fold[Double](Constant(Add), args, Constant(0.0))
        with DoubleTerm {
  def upperBound = args.foldLeft(0.0) {(b, a) => b + a.upperBound}

  override def ground(env: Env) = Sum(args.map(a => a.ground(env)))

  override def flatten: Sum[DoubleTerm] = Sum(args.flatMap(a => a match {case x: Sum[_] => x.flatten.args; case _ => Seq(a)}))

  override def toString = args.mkString("(", "+", ")")


  override def simplify: DoubleTerm = Sum(args.map(_.simplify))
}

case class SumOverGroundings[+T<:DoubleTerm](term:T,envs:Seq[Env])
        extends Sum[DoubleTerm](envs.map(term.ground(_)))

case class ProdOverGroundings[+T<:DoubleTerm](term:T,envs:Seq[Env])
        extends Multiplication[DoubleTerm](envs.map(term.ground(_)))

object SumHelper {
  def sum(terms: Collection[DoubleTerm], env: Env) = terms.foldLeft(0.0) {(s, t) => s + env(t)}
}

case class Multiplication[+T <: DoubleTerm](override val args: Seq[T]) extends Fold[Double](Constant(Times), args, Constant(1.0))
        with DoubleTerm {
  override def ground(env: Env) = Multiplication(args.map(a => a.ground(env)))

  def upperBound = args.foldLeft(1.0) {(b, a) => b * Math.abs(a.upperBound)}

  override def toString = args.mkString("(", "*", ")")

  //def flatten:Multiplication[DoubleTerm] = Multiplication(args.flatMap(a => a match {case x:Multiplication[_] => x.flatten.args; case _ => Seq(a) }))
  override def simplify: DoubleTerm = Multiplication(args.map(_.simplify))

}

case class QuantifiedSum[T](override val variable: Var[T], override val formula: DoubleTerm)
        extends Quantification(Constant(Add), variable, formula, Constant(0.0)) with DoubleTerm {
  override lazy val unroll = {
    val env = new MutableEnv
    Sum(variable.values.map(value => {env += variable -> value; formula.ground(env)}).toSeq)
  }

  def upperBound = unroll.upperBound

  override def ground(env: Env) = unroll.ground(env)

  override def simplify = QuantifiedSum(variable,formula.simplify)
}

case class QuantifiedMultiplication[T](override val variable: Var[T], override val formula: DoubleTerm)
        extends Quantification(Constant(Times), variable, formula, Constant(1.0)) with DoubleTerm {
  override lazy val unroll = {
    val env = new MutableEnv
    Multiplication(variable.values.map(value => {env += variable -> value; formula.ground(env)}).toSeq)
  }

  def upperBound = unroll.upperBound

  override def ground(env: Env) = unroll.ground(env)

  override def simplify = QuantifiedMultiplication(variable,formula.simplify)
}


case class Indicator(boolTerm: BooleanTerm) extends FunApp(Constant(CastBoolToDouble), boolTerm)
        with DoubleTerm {
  def upperBound = if (boolTerm.upperBound) 1.0 else 0.0

  override def ground(env: Env) = Indicator(boolTerm.ground(env))


  override def simplify = Indicator(boolTerm.simplify)

  override def toString = "[[" + boolTerm + "]]"
}

case class AlchemyIndicator(boolTerm: BooleanTerm)
        extends Sum(boolTerm.toCNF.args.map(a => TimesApp(Indicator(a), DoubleConstant(1.0 / boolTerm.toCNF.args.size)))) {
}

case class DoubleFunApp[T](override val function: Term[T => Double], override val arg: Term[T])
        extends FunApp(function, arg) with DoubleTerm {
  def upperBound = Math.POS_INF_DOUBLE

  override def simplify: DoubleTerm =
    function.simplify match {
      case Constant(f) => arg.simplify match {
        case Constant(x) => DoubleConstant(f(x));
        case x => DoubleFunApp(Constant(f), x)
      }
      case f => DoubleFunApp(f, arg.simplify)
    }

  override def ground(env: Env) = DoubleFunApp(function.ground(env), arg.ground(env))
}

case class DoubleConstant(override val value: Double) extends BoundedConstant(value) with DoubleTerm {
  //problem Constant superclass defines ground to return Constants

  override def ground(env: Env) = this


  override def simplify = this
}

case class Normalize(arg: DoubleTerm) extends DoubleTerm {
  def upperBound = 1.0

  //todo
  def values = Values(0.0, 1.0)

  //todo: simplify of DoubleTerm should return DoubleTerm
  def simplify = null //Normalize(arg.simplify)

  def eval(env: Env): Option[Double] = {
    var sum = 0.0
    Env.forall(variables) {
      x => {
        arg.eval(x) match {case None => return None; case Some(result) => sum += result;}
      }
    }
    arg.eval(env).map(_ / sum)
  }


  override def flatten = Normalize(arg.flatten)

  def ground(env: Env) = Normalize(arg.ground(env))

  def variables = arg.variables

  def subterms = Seq(arg)
}

case class Uniform(from: DoubleTerm, to: DoubleTerm) extends DoubleTerm {
  def ground(env: Env) = Uniform(from.ground(env), to.ground(env))

  def upperBound = to.upperBound

  //todo: need double values
  def values = null

  def simplify = Uniform(from.simplify, to.simplify)

  private val storedRandomValues = new HashMap[(Double, Double), Double]

  def eval(env: Env): Option[Double] = {
    val fromValue = from.eval(env)
    if (fromValue == None) return None
    val toValue = to.eval(env)
    if (fromValue == None) return None
    val scale = toValue.get - fromValue.get
    val offset = fromValue.get
    Some(storedRandomValues.getOrElseUpdate((fromValue.get, toValue.get), util.GlobalRandom.nextDouble * scale + offset))
  }

  def variables = from.variables ++ to.variables

  def subterms = Seq(from, to)


}

case class Objective(override val hidden:Set[EnvVar[_]],override val term:DoubleTerm)
        extends DependsOn[Double,DoubleTerm](term,hidden) with DoubleTerm {
  def upperBound: Double = term.upperBound


  override def ground(env: Env) = Objective(hidden,term.ground(env))

  override def simplify = Objective(hidden, term.simplify)
}

object Add extends (Double => (Double => Double)) {
  def apply(arg1: Double): (Double => Double) = (arg2: Double) => arg1 + arg2

  override def toString = "Add"
}

object Times extends (Double => (Double => Double)) {
  def apply(arg1: Double): (Double => Double) = (arg2: Double) => arg1 * arg2

  override def toString = "Times"
}

object CastBoolToDouble extends (Boolean => Double) {
  def apply(bool: Boolean) = if (bool) 1.0 else 0.0

  override def toString = "B2D"
}

object ExpOp extends (Double => Double) {
  def apply(arg: Double) = Math.exp(arg)

  override def toString = "exp"
}
