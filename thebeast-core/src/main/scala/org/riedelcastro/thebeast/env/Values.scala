package org.riedelcastro.thebeast.env


import booleans.Bools
import collection.mutable.{HashSet, MapProxy}
import ints.IntConstant
import org.riedelcastro.thebeast.util.{GlobalRandom, Util}
import tuples.{TupleValues2, TupleValues}

/**
 * @author Sebastian Riedel
 */
trait Values[+T] extends Iterable[T] {
  def defaultValue: T = elements.next

  def randomValue: T = {val seq = toSeq; seq(new scala.util.Random().nextInt(seq.size))}

  def randomValue(random: scala.util.Random): T = {val seq = toSeq; seq(random.nextInt(seq.size))}

  def createVariable(name: String): Var[T] = new Var(name, this)

  def compare[R>:T](x:R,y:R) = false

  def size: Int = toSeq.size

  def arity = this match {
    case v: TupleValues[_] => v.productArity
    case _ => 1
  }

  def argType(arg: Int): Values[Any] = this match {
    case v: TupleValues[_] => v.productElement(arg).asInstanceOf[Values[Any]]
    case _ => if (arg != 0) error("Not tuple values, argType makes no sense") else this
  }

  def validate[B >: T](value: B): Boolean = true

}

class MutableValues[T] extends HashSet[T] with Values[T] {
  override def size = super.size

  override def validate[B >: T](value: B): Boolean = {
    try {
      if (!contains(value.asInstanceOf[T]))
        this += value.asInstanceOf[T]
      true
    } catch {
      case _ =>
        error("Can't add " + value + " to this set of values because its type is not compatible")
    }
  }

  //todo: this should be something like Anyref.hashCode
  override lazy val hashCode = GlobalRandom.nextInt

  override def equals(obj: Any) = obj.isInstanceOf[AnyRef] && (this eq obj.asInstanceOf[AnyRef])
}

class IntRangeValues(val from: Int, val to: Int) extends Range(from, to + 1, 1) with Values[Int] {
  override def size = super.size
}

object Values {
  def apply[T](values: Collection[T]) = new ValuesProxy(Seq.empty ++ values)

  def apply[T](values: T*) =
    new ValuesProxy[T](values.foldLeft(Set[T]()) {(result, v) => result ++ Set(v)}.toList)
}

object Ints {
  def apply(values: Collection[Int]) = new ValuesProxy(Seq.empty ++ values){
    override def compare[R>:Int](x: R, y: R) = (x,y) match {
      case (x:Int,y:Int) => x < y
      case _ => false
    }
  }
}


class ValuesProxy[T](override val self: Iterable[T]) extends Values[T] with IterableProxy[T]



case class FunctionValues[T, R](val domain: Values[T], val range: Values[R]) extends Values[FunctionValue[T, R]] {
  def elements = Util.AllFunctions(domain.toStream, range.toStream).map(m => toFunctionValue(m)).elements

  override lazy val defaultValue = SingletonFunction(this, range.defaultValue)

  def toFunctionValue(map: Map[T, R]): FunctionValue[T, R] = {
    val f = new MutableFunctionValue[T, R](this)
    f ++= map
    f
  }

}

case class SingletonFunction[T, R](val signature: FunctionValues[T, R], val value: R) extends FunctionValue[T, R] {
  def getSources(r: Option[R]) = {
    r match {
      case Some(value) => signature.domain
      case _ => Set()
    }
  }

  def apply(t: T) = value
}


trait FunctionValue[T, R] extends (T => R) {
  def getSources(r: Option[R]): Iterable[T]

  def signature: FunctionValues[T, R]

  def countMatches(that: FunctionValue[T, R]): R => Int = {
    r => this.getSources(Some(r)).foldLeft(0) {
      (count, t) => count + (if (that.getSources(Some(r)).exists(x => x == t)) 1 else 0)
    }
  }
}

case class LessThanFunction[T](domain:Values[T]) extends FunctionValue[(T,T),Boolean]{

  val signature = new FunctionValues(TupleValues2(domain,domain),Bools)

  def apply(pair:(T,T)): Boolean = domain.compare(pair._1,pair._2)

  def getSources(r: Option[Boolean]): Iterable[(T,T)] = null
}

case class LessThan[T](domain:Values[T]) extends Constant(LessThanFunction(domain))


class MutableFunctionValue[T, R](val signature: FunctionValues[T, R])
        extends scala.collection.mutable.HashMap[T, R] with FunctionValue[T, R] {
  def getSources(r: Option[R]): Iterable[T] = {
    r match {
      case Some(x) => signature.domain.filter(d => get(d) == Some(x))
      case None => signature.domain.filter(d => !isDefinedAt(d))
    }
  }

  override def update(key: T, value: R) = {
    signature.domain.validate(key)
    signature.range.validate(value)
    super.update(key, value)
  }

  override def clone: MutableFunctionValue[T, R] = {
    val result = new MutableFunctionValue(signature)
    copyTo(result)
    result
  }

  def copyTo(result: scala.collection.mutable.HashMap[T, R]) = {
    foreach {
      case (key, value) =>
        if (value.isInstanceOf[MutableFunctionValue[_, _]])
          result += (key -> value.asInstanceOf[MutableFunctionValue[Any, Any]].clone.asInstanceOf[R])
        else
          result += (key -> value)
    }
  }


  override def equals(obj: Any): Boolean = obj match {
    case x: MutableFunctionValue[_, _] => super.equals(x) && x.signature == signature
    case _ => false
  }

  private class ClosedMutableMap(var self: MutableFunctionValue[T, R])
          extends MutableFunctionValue(self.signature) with MapProxy[T, R] {
    override def default(a: T) = signature.range.defaultValue

    override def apply(a: T) = self.get(a) match {
      case Some(x: MutableFunctionValue[_, _]) =>
        x.asInstanceOf[MutableFunctionValue[Any, Any]].close.asInstanceOf[R]

      case Some(_) => super.apply(a)
      case None => default(a)
    }

    override def equals(obj: Any): Boolean = obj match {
      case x: MutableFunctionValue[_,_]#ClosedMutableMap => super.equals(x) && x.signature == signature
      case _ => false
    }

    override def get(a: T) = {
      self.get(a) match {
        case Some(x: MutableFunctionValue[_, _]) =>
          Some(x.asInstanceOf[MutableFunctionValue[Any, Any]].close.asInstanceOf[R])

        case Some(_) => super.get(a)
        case None => Some(default(a))
      }
    }

    override def clone: ClosedMutableMap = {
      new ClosedMutableMap(self.clone)
    }

    override def getSources(r: Option[R]): Iterable[T] = {
      r match {
        case Some(x) =>
          signature.domain.filter(d => get(d) == Some(x))
        case None => Set[T]()
      }
    }

    override def close = this
  }

  def close: MutableFunctionValue[T, R] = {
    new ClosedMutableMap(this)
  }

}