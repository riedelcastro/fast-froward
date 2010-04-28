package org.riedelcastro.thebeast.util

/**
 * @author Sebastian Riedel
 */

trait SimpleNamed {
  override def toString = getClass.getSimpleName
}