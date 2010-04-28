package org.riedelcastro.thebeast.env.doubles

import collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: riedelcastro
 * Date: Nov 7, 2009
 * Time: 12:01:53 AM
 * To change this template use File | Settings | File Templates.
 */

trait DoubleTermOptimizer {
  def optimize(term:DoubleTerm):DoubleTerm
  def unapply(term:DoubleTerm):Option[DoubleTerm]
}

object DoubleTermOptimizer {

  val optimizers = new ArrayBuffer[DoubleTermOptimizer]

  def optimize(term:DoubleTerm):DoubleTerm = {
    term match {
      case DoubleTermOptimizer(optimized) => optimized
      case _ => term
    }
  }

  def unapply(term: DoubleTerm): Option[DoubleTerm] = {
    term match {
      case ExpWeightedDNFMatch(wdnf) => Some(wdnf);
      case ExpWeightedBooleanLiteralMatch(wlit) => Some(wlit);
      case _ => for (optimizer <- optimizers) optimizer.unapply(term).foreach(t => return Some(t)); None
    }
  }
}