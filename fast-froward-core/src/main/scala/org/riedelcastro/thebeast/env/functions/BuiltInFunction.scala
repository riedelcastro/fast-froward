package org.riedelcastro.thebeast.env.functions

/**
 * @author Sebastian Riedel
 */
trait BuiltInFunction {
    
}

class EQ[T] extends (T => (T => Boolean)) {
  def apply(lhs: T): (T => Boolean) = (rhs: T) => lhs == rhs;
  override def toString = "Equals"
}


case class BoundedConstant[T](override val value: T) extends Constant(value) with BoundedTerm[T] {
  //override type Type = BoundedConstant[T]
  override def upperBound = value
}

