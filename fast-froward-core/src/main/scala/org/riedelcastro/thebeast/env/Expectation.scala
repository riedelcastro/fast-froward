package org.riedelcastro.thebeast.env

/**
 * @author Sebastian Riedel  
 */

trait Expectation[T] {
  def value:T
  def add(prob:Double, t:T)
}