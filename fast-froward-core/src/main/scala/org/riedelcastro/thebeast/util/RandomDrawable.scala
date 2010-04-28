package org.riedelcastro.thebeast.util

import scala.util.Random


/**
 * @author Sebastian Riedel
 */

trait RandomDrawable[T] extends Seq[T] {
  def randomValue:T = this(GlobalRandom.nextInt(size))
}
