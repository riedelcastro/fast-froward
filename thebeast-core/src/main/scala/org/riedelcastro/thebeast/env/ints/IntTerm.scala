package org.riedelcastro.thebeast.env.ints

import org.riedelcastro.thebeast.env.{BoundedTerm, FunApp, Constant, Env}
import org.riedelcastro.thebeast.env.booleans.BooleanFunApp
import org.riedelcastro.thebeast.env.functions.BoundedConstant
/**
 * @author Sebastian Riedel
 */

trait IntTerm extends BoundedTerm[Int] {

  def < (that:IntTerm) = BooleanFunApp(FunApp(Constant(IntLT),this), that)
}


case class IntConstant(override val value: Int) extends BoundedConstant(value) with IntTerm {
  override def ground(env: Env) = this

  override def equals(obj: Any): Boolean = obj match {
    case Constant(t) => t == value
    case _ => false
  }
}

object IntAdd extends (Int => (Int => Int)) {
  def apply(arg1: Int): (Int => Int) = (arg2: Int) => arg1 + arg2

  override def toString = "IntAdd"
}

object IntLT extends (Int => (Int => Boolean)) {
  def apply(arg1: Int): (Int => Boolean) = (arg2: Int) => arg1 < arg2

  override def toString = "IntLT"
}

