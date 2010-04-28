package org.riedelcastro.thebeast.env.tuples


import org.riedelcastro.thebeast.util.Util
import org.riedelcastro.thebeast.env.Values

/**
 * @author Sebastian Riedel
 */

trait TupleValues[T] extends Product with Values[T]{
  override def validate[B >: T](value: B):Boolean = {
    value match {
      case x:Product => {
        if (x.productArity != productArity) error("Product must have the same arity as the TupleValues")
        for (i <- 0 until productArity) productElement(i).asInstanceOf[Values[Any]].validate(x.productElement(i))
        true
      }
      case _ => error("Only products can be values of a TupleValues")
    }
  }
}

case class TupleValues2[T1, T2](_1: Values[T1], _2: Values[T2]) extends TupleValues[Tuple2[T1, T2]]  {
  def elements = Util.Cartesian.cartesianProduct(Seq(_1.toStream, _2.toStream)).map(
    seq => Tuple2(seq(0).asInstanceOf[T1], seq(1).asInstanceOf[T2])).elements

  def x[T3](_3: Values[T3]) = TupleValues3(_1, _2, _3)

}
case class TupleValues3[T1, T2, T3](_1: Values[T1], _2: Values[T2], _3: Values[T3]) extends TupleValues[Tuple3[T1, T2, T3]] {
  def elements = Util.Cartesian.cartesianProduct(Seq(_1.toStream, _2.toStream, _3.toStream)).map(
    seq => Tuple3(
      seq(0).asInstanceOf[T1],
      seq(1).asInstanceOf[T2],
      seq(2).asInstanceOf[T3]))
          .elements

  def x[T4](_4: Values[T4]) = TupleValues4(_1, _2, _3, _4)

}

case class TupleValues4[T1, T2, T3, T4](_1: Values[T1], _2: Values[T2], _3: Values[T3], _4: Values[T4])
        extends TupleValues[Tuple4[T1, T2, T3, T4]] {
  def elements = Util.Cartesian.cartesianProduct(Seq(_1.toStream, _2.toStream, _3.toStream, _4.toStream)).map(
    seq => Tuple4(
      seq(0).asInstanceOf[T1],
      seq(1).asInstanceOf[T2],
      seq(2).asInstanceOf[T3],
      seq(3).asInstanceOf[T4]))
          .elements

  def x[T5](_5: Values[T5]) = TupleValues5(_1, _2, _3, _4, _5)

}

case class TupleValues5[T1, T2, T3, T4, T5](_1: Values[T1], _2: Values[T2], _3: Values[T3], _4: Values[T4], _5: Values[T5])
        extends TupleValues[Tuple5[T1, T2, T3, T4, T5]] {
  def elements = Util.Cartesian.cartesianProduct(Seq(_1.toStream, _2.toStream, _3.toStream, _4.toStream, _5.toStream)).map(
    seq => Tuple5(
      seq(0).asInstanceOf[T1],
      seq(1).asInstanceOf[T2],
      seq(2).asInstanceOf[T3],
      seq(3).asInstanceOf[T4],
      seq(4).asInstanceOf[T5]))
          .elements
}
