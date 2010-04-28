package org.riedelcastro.thebeast.env.tuples

import org.riedelcastro.thebeast.env._

/**
 * @author Sebastian Riedel
 */

trait TupleTerm extends scala.Product {

  def subterms: Seq[Term[Any]] = {
    for (i <- 0 until productArity) yield productElement(i).asInstanceOf[Term[Any]]
  }
  override def toString = (for (i <- 0 until productArity) yield productElement(i)).mkString(",")
}

object TupleTerm {
  def apply(args:Term[Any]*) : Term[Any] = args.size match {
    case 1 => args(0)
    case 2 => TupleTerm2(args(0),args(1))
    case 3 => TupleTerm3(args(0),args(1),args(2))
    case _ => error("Can't do tuples with more than 3 arguments yet")
  }
  //def apply(args:Term[Any]*): Term[Any] = apply(args.toSeq)
}




case class TupleTerm2[T1,T2](_1:Term[T1],_2:Term[T2]) extends Term[Tuple2[T1,T2]] with TupleTerm {
  def eval(env: Env) = {
    val arg1: Option[T1] = _1.eval(env)
    val arg2: Option[T2] = _2.eval(env)
    if (arg1 != None && arg2 != None) Some(Tuple2(arg1.get, arg2.get)) else None
  }

  def values = TupleValues2(_1.values,_2.values)

  def ground(env: Env) = TupleTerm2(_1.ground(env),_2.ground(env))

  def simplify = if (_1.isInstanceOf[Constant[_]] && _2.isInstanceOf[Constant[_]])
    Constant(Tuple2(_1.asInstanceOf[Constant[T1]].value,_2.asInstanceOf[Constant[T2]].value ))
    else this

  def variables = _1.variables ++ _2.variables
}

case class TupleTerm3[T1,T2,T3](_1:Term[T1],_2:Term[T2],_3:Term[T3]) extends Term[Tuple3[T1,T2,T3]] with TupleTerm {
  def eval(env: Env) = {
    val arg1: Option[T1] = _1.eval(env)
    val arg2: Option[T2] = _2.eval(env)
    val arg3: Option[T3] = _3.eval(env)
    if (arg1 != None && arg2 != None && arg3 != None) Some(Tuple3(arg1.get, arg2.get,arg3.get)) else None
  }

  def values = TupleValues3(_1.values,_2.values, _3.values)

  def ground(env: Env) = TupleTerm3(_1.ground(env),_2.ground(env), _3.ground(env))

  def simplify = if (_1.isInstanceOf[Constant[_]] && _2.isInstanceOf[Constant[_]] && _3.isInstanceOf[Constant[_]])
    Constant(Tuple3(_1.asInstanceOf[Constant[T1]].value,_2.asInstanceOf[Constant[T2]].value, _3.asInstanceOf[Constant[T3]].value))
    else this

  def variables = _1.variables ++ _2.variables ++ _3.variables
}

 