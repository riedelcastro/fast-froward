package org.riedelcastro.thebeast.env


import org.specs._
import runner.JUnit4

/**
 * @author Sebastian Riedel
 */
class MutableFunctionTest extends JUnit4(MutableFunctionValueSpecification)
object MutableFunctionValueSpecification extends Specification with TheBeastEnv {
  "A mutable function value" should {
    "store a mapping" in {
      val f = new MutableFunctionValue(Values(1,2,3)->Values("A","B"))
      f(1) = "A"
      f(1) must_== "A"
    }
    "store a mapping for tuples" in {
      val f = new MutableFunctionValue((Values(1,2) x Values(1,2) x Values(1,2))->Values("A","B"))
      f(Tuple3(1,2,1)) = "A"
      f(1,2,1) must_== "A"
    }
    "return None for keys that have not been set" in {
      val f = new MutableFunctionValue(Values(1,2,3)->Values("A","B"))
      f(1) = "A"
      f.get(2) must_== None
    }
    "provide a closed function that returns default values for unset keys" in {
      val f = new MutableFunctionValue(Values(1,2,3)->Values("A","B"))
      f(1) = "B"
      f.close(2) must_== "A"
    }
    "provide a closed function returns the correct sources for the default range element" in {
      val f = new MutableFunctionValue(Values(1,2,3)->Values("A","B")).close
      f(1) = "B"
      val sources = f.getSources(Some("A"))
      //sources mu
      //sources  3
      //todo this spec
      1 must_== 1
      
      //f.close(2) must_== "A"
    }
    "provide a closed function that recursively returns closed functions" in {
      val f = new MutableFunctionValue(Values(1,2)->(Values(1,2)->Values("A","B")))
      val g = new MutableFunctionValue(Values(1,2)->Values("A","B"))
      g(2) = "B"
      f(1) = g
      val closed = f.close
      closed(1)(1) must_== "A"
      closed(2)(1) must_== "A"
      closed(2)(2) must_== "A"
      closed(1)(2) must_== "B"
    }

    "return all sources that map to a specific return value in" in {
      val f = new MutableFunctionValue(Values(1,2,3)->Values("A","B"))
      f(1) = "B"
      f.getSources(Some("B")) must_== List(1)
    }
    "return all sources that are not maped to any value in" in {
      val f = new MutableFunctionValue(Values(1,2,3)->Values("A","B"))
      f(1) = "B"
      f.getSources(None) must_== List(2,3)
    }

    "should count arguments mapped to the same element" in {
      val f1 = new MutableFunctionValue((Values(1,2) x Values(1,2)) ->Values("A","B")).close
      f1((1,2)) = "B"
      f1((2,1)) = "B"

      val f2 = new MutableFunctionValue((Values(1,2) x Values(1,2)) ->Values("A","B")).close
      f2((1,2)) = "B"
      f2((1,1)) = "B"

      f1.countMatches(f2)("A") must_== 1 // (2,2)
      f1.countMatches(f2)("B") must_== 1 // (1,2)

    }


  }

}