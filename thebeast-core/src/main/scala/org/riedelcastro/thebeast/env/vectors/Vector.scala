package org.riedelcastro.thebeast.env.vectors


import collection.mutable.{HashMap}
import util.SimpleNamed
import org.riedelcastro.thebeast.util.SimpleNamed
import org.riedelcastro.thebeast.env.{TheBeastEnv, Values}

/**
 * @author Sebastian Riedel
 */

class Vector {
  private val store = new HashMap[Any, Double] {
    override def default(a: Any) = {
      a match {

        case seq: Seq[_] => defaultsForFirstKey(seq(0))
        case _ => defaultsForFirstKey(a)
      }
    }
  }

  private val defaultsForFirstKey = new HashMap[Any, Double] {
    override def default(a: Any) = defaultValue
  }

  var defaultValue = 0.0

  def setDefaultForFirstKey(key: Any, value: Double) = defaultsForFirstKey(key) = value

  def set(value: Double, keys: Any*) {
    store += (keys.toList -> value)
  }

  def update(key1:Any, value:Double) = set(value, key1)
  def update(key1:Any, key2:Any, value:Double) = set(value, key1, key2)
  def update(key1:Any, key2:Any, key3:Any, value:Double) = set(value, key1, key2, key3)

  def setWithSingleKey(value: Double, key: Any) {
    store += (key -> value)
  }


  private def unpackIfSingle(args: Any*) = if (args.length == 1) args(0) else args

  def get(keys: Any*): Double = store(keys.toList)

  def getWithSingleKey(key: Any): Double = store.getOrElse(key, 0.0)


  def add(that: Vector, scale: Double): Vector = {
    val result = new Vector
    result.addInPlace(this, 1.0)
    result.addInPlace(that, scale)
    result
  }

  def scalar(scale: Double): Vector = {
    val result = new Vector
    result.addInPlace(this, scale)
    result
  }

  def addInPlace(that: Vector, scale: Double): Unit = {
    for (entry <- store.elements)
      setWithSingleKey(entry._2 + scale * that.getWithSingleKey(entry._1), entry._1)
    for (entry <- that.store.elements)
      if (!store.keySet.contains(entry._1)) store += (entry._1 -> entry._2 * scale)
  }

  def dot(that: Vector): Double = {
    store.foldLeft(0.0) {(score, keyValue) => score + keyValue._2 * that.getWithSingleKey(keyValue._1)}
  }


  override def toString =
    store.elements.foldLeft("") {
      (s, e) =>
              s + e._1.asInstanceOf[Collection[_]].map("%-6s".format(_)).mkString(" ") + "\t" + e._2.toString + "\n"
    }
}

object VectorValues extends Values[Vector] {
  def elements = error("Can't iterate over infinite set of all vector values")
}

case object VectorAdd extends (Vector => (Vector => Vector)) with SimpleNamed {
  def apply(lhs: Vector) = (rhs: Vector) => lhs.add(rhs, 1.0)


}

case object VectorDot extends (Vector => (Vector => Double)) with SimpleNamed {
  def apply(lhs: Vector) = (rhs: Vector) => lhs.dot(rhs)
}

case object VectorScalar extends (Vector => (Double => Vector)) with SimpleNamed {
  def apply(lhs: Vector) = (rhs: Double) => lhs.scalar(rhs)
}

object VectorSpace extends Values[Vector] {
  def elements = throw new Error("Can't iterate over all vectors")

  override def defaultValue = VectorZero

  override def randomValue = throw new Error("Space too large for randomly drawing an element")
}

object VectorZero extends Vector {
  override def addInPlace(that: Vector, scale: Double) = throw new Error("Cannot change the zero vector")
}

object VectorDemo extends Application with TheBeastEnv {
  val vector = new Vector
  vector.set(2.0, "blah", 1)
  vector.set(-1.0, 200, "pups", true)

  println(vector)

  //val Bools = Values(true, false)  
  val Persons = Values("Anna", "Peter", "Nick", "Ivan")
  val smokes = "smokes" <~ Persons -> Bools;
  val cancer = "cancer" <~ Persons -> Bools;
  val friends = "friends" <~ Persons -> (Persons -> Bools);

  val weights = "w" <~ VectorSpace

  val f1 = sum(Persons) {x => $ {smokes(x) -> cancer(x)} * 0.1}
  val f2 = sum(Persons) {x => sum(Persons) {y => $ {friends(x)(y) && smokes(x) -> smokes(y)} * 0.1}}
  val f3 = vectorSum(Persons) {x => $ {smokes(x) -> cancer(x)} * UnitVector(x)}
  //val mln = f3 dot weights

  //it should be possible to move the dot product into the summation, replacing UnitVector(x) with weights(x)

}

