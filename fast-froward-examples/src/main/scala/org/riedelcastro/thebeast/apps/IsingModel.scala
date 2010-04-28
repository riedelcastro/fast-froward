package org.riedelcastro.thebeast.apps


import org.riedelcastro.thebeast.env.doubles.Uniform
import org.riedelcastro.thebeast.env.{TheBeastImplicits, Predicate, Ints}
import org.riedelcastro.thebeast.solve.{ExhaustiveMarginalInference, SumProductBeliefPropagation}
import org.riedelcastro.thebeast.util.{TimingCollector, Trackers}

/**
 * @author Sebastian Riedel
 */

object IsingModel {
  import TheBeastImplicits._

  def main(args: Array[String]):Unit = {
    Trackers += TimingCollector
    val n = 3
    val m = 3
    val Rows = Ints(0 until n)
    val Cols = Ints(0 until m)
    val node = Predicate("node", Rows x Cols)
    val ising = normalize(exp(sum(Rows, Cols) {(x, y) => $(node(x, y)) * Uniform(-2.0,2.0)} +
            sum(Ints(0 until n - 1), Cols) {(x, y) => $(node(x, y) <~> node(x + 1, y)) * Uniform(-2.0,2.0)} +
            sum(Rows, Ints(0 until m - 1)) {(x, y) => $(node(x, y) <~> node(x, y + 1)) * Uniform(-2.0,2.0)}))
//    val ising = normalize(exp(sum(Rows, Cols) {(x, y) => $(node(x, y)) * 1.0} +
//            sum(Ints(0 until n - 1), Cols) {(x, y) => $(node(x, y) <~> node(x + 1, y)) * 1.0} +
//            sum(Rows, Ints(0 until m - 1)) {(x, y) => $(node(x, y) <~> node(x, y + 1)) * 1.0}))

    //now what? run sum product
    val solver = new SumProductBeliefPropagation()
    val beliefs = solver.infer(ising)
    println(beliefs)
    println(solver.iterations)
    println(ExhaustiveMarginalInference.infer(ising))
    println(TimingCollector.timings)

    null
  }
}