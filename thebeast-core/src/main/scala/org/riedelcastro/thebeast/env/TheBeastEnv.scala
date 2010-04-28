package org.riedelcastro.thebeast.env

import combinatorics.SpanningTreeConstraint
import doubles._
import ints._
import booleans._
import tuples._
import functions._
import vectors._

/**
 * @author Sebastian Riedel
 */

object TheBeastImplicits extends TheBeastEnv {
}

trait TermShortCuts {
  def $(term: BooleanTerm) = Indicator(term)

  def $$(term: BooleanTerm) = AlchemyIndicator(term)

  def unit(key: Term[Any]*) = UnitVector(key: _*)

  def exp(arg: DoubleTerm) = Exp(arg)

  def normalize(arg: DoubleTerm) = Normalize(arg)

  def ^[T](t: T) = Constant(t)

  def ptree[V](edges: Term[FunctionValue[(V, V), Boolean]],
               vertices: Term[FunctionValue[V, Boolean]],
               root: Term[V],
               order: Term[FunctionValue[(V, V), Boolean]]) = SpanningTreeConstraint(edges, vertices, root, order)
}

trait GenericTermConversion {

  implicit def genericTerm2booleanFunAppBuilder[T](fun: Term[FunctionValue[T, Boolean]]) = new (Term[T] => BooleanFunApp[T]) {
    def apply(t: Term[T]) = BooleanFunApp(fun, t)
  }

  implicit def tuple2term2booleanFunAppBuilder[T1, T2](fun: Term[FunctionValue[(T1, T2), Boolean]]) = new (TupleTerm2[T1, T2] => BooleanFunApp[(T1, T2)]) {
    def apply(t: TupleTerm2[T1, T2]) = BooleanFunApp(fun, t)

    def apply(t1: Term[T1], t2: Term[T2]) = BooleanFunApp(fun, TupleTerm2(t1, t2))
  }

  implicit def tuple3term2booleanFunAppBuilder[T1, T2, T3](fun: Term[FunctionValue[(T1, T2, T3), Boolean]]) = new (TupleTerm3[T1, T2, T3] => BooleanFunApp[(T1, T2, T3)]) {
    def apply(t: TupleTerm3[T1, T2, T3]) = BooleanFunApp(fun, t)

    def apply(t1: Term[T1], t2: Term[T2], t3: Term[T3]) = BooleanFunApp(fun, TupleTerm3(t1, t2, t3))
  }
  

}

object GenericImplicits extends QuantificationShortCuts with TermShortCuts with GenericTermConversion

trait TheBeastEnv extends QuantificationShortCuts with TermShortCuts {
  val Bools = booleans.Bools
  //val Bools = Values(false,true)


  implicit def string2varbuilder(name: String) = new {
    def <~[T](values: Values[T]) = Var(name, values)

    //def in[T, R](values: FunctionValues[T, R]) = FunVar(name, values)
  }

  implicit def varDouble2DoubleVar(varDouble: Var[Double]) = DoubleVar(varDouble.name, varDouble.values)

  //def ground(variable:Var[T], t:T)

  implicit def termToTermBuilder[T](term: Term[T]) = TermBuilder(term)

  def genericTerm2booleanFunAppBuilder[T](fun: Term[FunctionValue[T, Boolean]]) = new (Term[T] => BooleanFunApp[T]) {
    def apply(t: Term[T]) = BooleanFunApp(fun, t)
  }





  //implicit def value2constant[T](value: T) = Constant(value)

  implicit def function2constant[T1, R](value: T1 => R) = Constant(value)

  implicit def function22constant[T1, T2, R](value: (T1, T2) => R) = Constant(value)


  implicit def tuple2toTupleTerm2[T1, T2](value: (Term[T1], Term[T2])) = TupleTerm2(value._1, value._2)

  //  implicit def termToConditionedTerm1Builder[T,C1](term:Term[T]) = new {
  //    def | (condition:Term[C1]) = ConditionedTerm(term,condition)
  //  }

  //  implicit def termToConditionedTerm2Builder[T,C1,C2](term:Term[T]) = new {
  //    def | (c1:Term[C1], c2:Term[C2]) = ConditionedTerm(term,TupleTerm2(c1,c2))
  //  }


  implicit def term2SingletonConditionedTerm[T](term: Term[T]) = new ConditionedTerm(term, Singleton)

  implicit def term2ConditionedTermBuilder[T](term: Term[T]) = new {

    /**
     * Creates a conditioned term, useful for CPDs.
     */
    def |||[C1](condition: Term[C1]): ConditionedTerm[T, C1] = ConditionedTerm(term, condition)

    /**
     * Creates a conditioned term, useful for CPDs.
     */
    def |||[C1, C2](c1: Term[C1], c2: Term[C2]): ConditionedTerm[T, Tuple2[C1, C2]] = ConditionedTerm(term, TupleTerm2(c1, c2))
  }

  implicit def tupleterm2toConditionedTermBuilder[T1, T2](term: Tuple2[Term[T1], Term[T2]]) = new {

    /**
     * Creates a conditioned term, useful for CPDs.
     */
    def |||[C1](condition: Term[C1]): ConditionedTerm[(T1, T2), C1] =
      ConditionedTerm(TupleTerm2(term._1, term._2), condition)

    /**
     * Creates a conditioned term, useful for CPDs.
     */
    def |||[C1, C2](c1: Term[C1], c2: Term[C2]): ConditionedTerm[(T1, T2), Tuple2[C1, C2]] =
      ConditionedTerm(TupleTerm2(term._1, term._2), TupleTerm2(c1, c2))
  }

  //implicit def termtuple2toTupleTerm2[T1,T2](tuple: Tuple2[Term[T1],Term[T2]]) = TupleTerm2(tuple._1, tuple._2)

  implicit def double2constant(value: Double) = DoubleConstant(value)

  implicit def int2constant(value: Int) = IntConstant(value)

  implicit def string2constant(value: String) = Constant(value)

  implicit def bool2constant(value: Boolean) = BooleanConstant(value)


  case class FunAppVarBuilder[T, R](val funvar: EnvVar[T => R]) {
    def of(t: T) = FunAppVar(funvar, t)
  }

  case class TermBuilder[T](val term: Term[T]) {
    def ===(rhs: Term[T]): BooleanTerm = BooleanFunApp(FunApp(Constant(new EQ[T]), term), rhs)
  }

  case class FunctionValuesBuilder[T, R](domain: Values[T]) {
    def ->[R](range: Values[R]) = new FunctionValues(domain, range)

    def x[T2](other: Values[T2]) = TupleValues2(domain, other)

  }

  implicit def funvar2funAppVarBuilder[T, R](funvar: EnvVar[T => R]) = FunAppVarBuilder(funvar)


  implicit def term2funAppBuilder[T, R](fun: Term[T => R]) = new (Term[T] => FunApp[T, R]) {
    def apply(t: Term[T]) = FunApp(fun, t)
  }

  //  implicit def term2groundFunAppBuilder[T, R](fun: FunctionVar[T,R]) = new (T => GroundFunApp[T, R]) {
  //    def apply(t: T) = GroundFunApp(fun, t)
  //  }

  implicit def term2doubleFunAppBuilder[T](fun: Term[T => Double]) = new (Term[T] => DoubleFunApp[T]) {
    def apply(t: Term[T]) = DoubleFunApp(fun, t)
  }

  //  implicit def term2booleanFunAppBuilder[T >: AnyVal](fun: Term[T => Boolean]) = new (Term[T] => BooleanFunApp[T]) {
  //    def apply(t: Term[T]) = BooleanFunApp(fun, t)
  //  }

  implicit def term2booleanFunAppBuilder(fun: Term[Any => Boolean]) = new (Term[Any] => BooleanFunApp[Any]) {
    def apply(t: Term[Any]) = BooleanFunApp(fun, t)
  }


  implicit def intTerm2booleanFunAppBuilder(fun: Term[FunctionValue[Int, Boolean]]) =
    genericTerm2booleanFunAppBuilder(fun)

  //  implicit def anyterm2booleanFunAppBuilder[T1, T2](fun: Term[FunctionValue[Any, Boolean]]) = new {
  //    def apply(t: TupleTerm2[Any, Any]) = BooleanFunApp(fun, t)
  //    def apply(t1: Term[Any], t2: Term[Any]) = BooleanFunApp(fun, TupleTerm2(t1, t2))
  //  }


  implicit def tuple2term2booleanFunAppBuilder[T1, T2](fun: Term[FunctionValue[(T1, T2), Boolean]]) = new (TupleTerm2[T1, T2] => BooleanFunApp[(T1, T2)]) {
    def apply(t: TupleTerm2[T1, T2]) = BooleanFunApp(fun, t)

    def apply(t1: Term[T1], t2: Term[T2]) = BooleanFunApp(fun, TupleTerm2(t1, t2))
  }

  implicit def tuple3term2booleanFunAppBuilder[T1, T2, T3](fun: Term[FunctionValue[(T1, T2, T3), Boolean]]) = new (TupleTerm3[T1, T2, T3] => BooleanFunApp[(T1, T2, T3)]) {
    def apply(t: TupleTerm3[T1, T2, T3]) = BooleanFunApp(fun, t)

    def apply(t1: Term[T1], t2: Term[T2], t3: Term[T3]) = BooleanFunApp(fun, TupleTerm3(t1, t2, t3))
  }


  implicit def stringterm2booleanFunAppBuilder(fun: Term[String => Boolean]) = new (Term[String] => BooleanFunApp[String]) {
    def apply(t: Term[String]) = BooleanFunApp(fun, t)
  }

  implicit def varWithEnv2mapToBuilder[T, R](varWithEnv: VarWithEnv[FunctionValue[T, R]]) =
    MapToBuilder(varWithEnv.envVar, varWithEnv.env)

  //  implicit def term2arity2funAppBuilder[T1,T2, R](fun: Term[T1 => (T2=>R)]) = new ((Term[T1],Term[T2]) => FunApp[T2, R]) {
  //    def apply(t: (Term[T1],Term[T2])) = FunApp(FunApp(fun, t._1), t._2)
  //  }


  //  implicit def term2eqBuilder[T](lhs: Term[T]) = new {
  //    def ===(rhs: Term[T]) = TermEq(lhs, rhs)
  //  }

  implicit def values2FunctionValuesBuilder[T, R](domain: Values[T]): FunctionValuesBuilder[T, R] =
    FunctionValuesBuilder[T, R](domain)



  //  implicit def term2envVar[T](env:Term[T]): EnvVar[T] = {
  //    env match {
  //      case FunApp(f,Constant(v)) => FunAppVar(term2envVar(f),v)
  //      case _=> null
  //    }
  //  }

  implicit def intTerm2IntAppBuilder(lhs: Term[Int]) = new {
    def +(rhs: Term[Int]) = FunApp(FunApp(Constant(IntAdd), lhs), rhs)

    def <(rhs: Term[Int]) = BooleanFunApp(FunApp(Constant(IntLT), lhs), rhs)
  }

  implicit def doubleTerm2DoubleTermBuilder(lhs: DoubleTerm) = new {
    def +(rhs: DoubleTerm) = AddApp(lhs, rhs)

    def *(rhs: DoubleTerm) = TimesApp(lhs, rhs)
  }


  implicit def boolTerm2BoolAppBuilder(lhs: BooleanTerm) = new {
    def @@ = Indicator(lhs)

    def &&(rhs: BooleanTerm) = AndApp(lhs, rhs)

    def ->(rhs: BooleanTerm) = ImpliesApp(lhs, rhs)

    def -->(rhs: BooleanTerm) = ImpliesApp(lhs, rhs)

  }


}

trait QuantificationShortCuts {
  private var varCount = 0;


  private def createVariable[T](values: Values[T]): Var[T] = {
    varCount += 1;
    values.createVariable("x_" + varCount.toString)
  }

  def sum[T](values: Values[T])(formula: Var[T] => DoubleTerm) = {
    val variable = createVariable(values)
    QuantifiedSum(variable, formula(variable))
  }

  def sum[T1, T2](values1: Values[T1], values2: Values[T2])(formula: (Var[T1], Var[T2]) => DoubleTerm): QuantifiedSum[T1] =
    sum(values1) {x1 => sum(values2) {x2 => formula(x1, x2)}}


  def prod[T](values: Values[T])(formula: Var[T] => DoubleTerm) = {
    val variable = createVariable(values)
    QuantifiedMultiplication(variable, formula(variable))
  }

  def prod[T1, T2](values1: Values[T1], values2: Values[T2])(formula: (Var[T1], Var[T2]) => DoubleTerm): QuantifiedMultiplication[T1] =
    prod(values1) {x1 => prod(values2) {x2 => formula(x1, x2)}}


  def prod[T1, T2, T3](values1: Values[T1], values2: Values[T2], values3: Values[T3])(
          formula: (Var[T1], Var[T2], Var[T3]) => DoubleTerm): QuantifiedMultiplication[T1] =
    prod(values1) {x1 => prod(values2) {x2 => prod(values3) {x3 => formula(x1, x2, x3)}}}


  def vectorSum[T](values: Values[T])(formula: Var[T] => VectorTerm) = {
    val variable = createVariable(values)
    QuantifiedVectorSum(variable, formula(variable))
  }

  def vectorSum[T1, T2](values1: Values[T1], values2: Values[T2])
                       (formula: (Var[T1], Var[T2]) => VectorTerm): QuantifiedVectorSum[T1] =
    vectorSum(values1) {x1 => vectorSum(values2) {x2 => formula(x1, x2)}}

  def vectorSum[T1, T2, T3](v1: Values[T1], v2: Values[T2], v3: Values[T3])
                           (formula: (Var[T1], Var[T2], Var[T3]) => VectorTerm): QuantifiedVectorSum[T1] =
    vectorSum(v1) {x1 => vectorSum(v2, v3) {(x2, x3) => formula(x1, x2, x3)}}

  def vectorSum[T1, T2, T3, T4](v1: Values[T1], v2: Values[T2], v3: Values[T3], v4: Values[T4])
                               (formula: (Var[T1], Var[T2], Var[T3], Var[T4]) => VectorTerm): QuantifiedVectorSum[T1] =
    vectorSum(v1) {x1 => vectorSum(v2, v3, v4) {(x2, x3, x4) => formula(x1, x2, x3, x4)}}

  def forall[T](values: Values[T])(formula: Var[T] => BooleanTerm) = {
    val variable = createVariable(values)
    Forall(variable, formula(variable))
  }

  def forall[T1, T2](v1: Values[T1], v2: Values[T2])
                    (formula: (Var[T1], Var[T2]) => BooleanTerm): Forall[T1] = {
    forall(v1) {x1 => forall(v2) {x2 => formula(x1, x2)}}
  }

  def forall[T1, T2, T3](v1: Values[T1], v2: Values[T2], v3: Values[T3])
                        (formula: (Var[T1], Var[T2], Var[T3]) => BooleanTerm): Forall[T1] = {
    forall(v1) {x1 => forall(v2) {x2 => forall(v3) {x3 => formula(x1, x2, x3)}}}
  }


}