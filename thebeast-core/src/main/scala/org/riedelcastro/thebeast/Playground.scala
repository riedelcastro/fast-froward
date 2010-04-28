package org.riedelcastro.thebeast


import env._
import ints._
import solve.{GeneralizedMaxWalkSAT, ExhaustiveSearch}
/**
 * @author Sebastian Riedel
 */

object Playground extends Application with TheBeastEnv {
  val Ints = Values[Int](1, 2, 3)
  //val Bools = Values(false, true)
  val b = "b" <~ Bools
  val x = "x" <~ Ints
  val f = "f" <~ Ints -> Ints
  val pred = "pred" <~ Ints -> Bools
  val k = "k" <~ Ints -> (Ints -> Ints)
  val env = new MutableEnv
  println(env.eval(x))
  env += x -> 1
  env += (f of 1) -> 2
  env += (f of 2) -> 3
  env += ((k of 1) of 2) -> 3
  println(env.eval(x))
  println(env(FunApp(f, 1)))
  println(env(f(f(x))))
  println(env(k(1)(2)))
  println(env(IntAdd))
  println(env(^(IntAdd)(x)(1)))
  println(env(^(1) + x))

  println(Fold(IntAdd, Seq[Term[Int]](1, 2, x + 3, 4), 0))

  println((k(1)(2) + x).variables)
  println(env(k(1)(2) + x))

  println(Quantification(IntAdd, x, f(x), 0).unroll)
  //println(intSum(Ints) {x => f(x)})

  println(forall(Ints) {x => f(x) === 1})
  println(sum(Ints) {x => {f(x) === 1} @@})
  println((forall(Ints) {x => f(x) === 1}).variables)
  println(forall(Ints) {x => forall(Ints) {y => k(x)(y) === 1}})
  println(forall(Ints, Ints) {(x, y) => k(x)(y) === 1})
  println((forall(Ints) {x => forall(Ints) {y => k(x)(y) === 1}}).unroll)

  println(f(x).variables)
  //val env = MutableEnv
  //val f = "f" <~ FunctionValues(Set(1,2,3),Set(1,2))
  //env += (f->Map(1->2))
  //env += (f(1)->2)

  println(ExhaustiveSearch.search(f(x)).eval(f(x)))
  println(ExhaustiveSearch.argmax(sum(Ints) {x => $ {f(x) === 1} * 0.1}).result.eval(f(2)))

  val mws = new GeneralizedMaxWalkSAT
  println(mws.argmax((sum(Ints) {x => $ {f(x) === 2} * 0.1}).unroll).result.eval(f(2)))

  val Persons = Values("Anna", "Peter", "Nick", "Ivan")
  val smokes = "smokes" <~ Persons -> Bools;
  val cancer = "cancer" <~ Persons -> Bools;
  val friends = "friends" <~ Persons -> (Persons -> Bools);

  val f1 = sum(Persons) {x => $ {smokes(x) -> cancer(x)} * 0.1}
  val f2 = sum(Persons) {x => sum(Persons) {y => $ {friends(x)(y) && smokes(x) -> smokes(y)} * 0.1}}

  val mln = f1 + f2

  val Citations = Values("A","B","C")
  val same = "same" <~ (Citations x Citations) -> Bools
  val reflex = sum(Citations,Citations) {(x,y)=> ${same(x,y) -> same(y,x)}}


}