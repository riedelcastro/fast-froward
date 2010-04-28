package org.riedelcastro.thebeast.solve


import env.{Env, Term}

/**
 * @author Sebastian Riedel
 */

trait SatisfiabilitySolver {

  def satisfy(term: Term[Boolean]) : Env

}