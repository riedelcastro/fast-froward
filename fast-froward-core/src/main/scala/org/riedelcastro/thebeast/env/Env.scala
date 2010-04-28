package org.riedelcastro.thebeast.env


import collection.mutable.{HashMap, HashSet}
import util.Util
import org.riedelcastro.thebeast.util.Util

/**
 * @author Sebastian Riedel
 */
trait Env {
  def apply[T](term: Term[T]): T = term.eval(this).get

  //todo: should this be removed?
  //todo: should we 
  def eval[T](term: Term[T]): Option[T] = term.eval(this)

  def resolveVar[T](variable: EnvVar[T]): Option[T]

  def mask(hiddenVariables: Set[EnvVar[_]]) = new MaskedEnv(this, hiddenVariables);

  /**
   * uses the bindings of the overlayed env for all the variables it has bindings for, and otherwise
   * the bindings of this env.
   */
  def overlay(over: Env) = new OverlayedEnv(this, over)

  def variables: Set[EnvVar[_]]
  
}

object EmptyEnv extends Env {
  def variables = Set()

  def resolveVar[T](variable: EnvVar[T]) = None
}


object Env {
  def forall(variables:Collection[EnvVar[Any]])(procedure:MutableEnv=>Unit) {
    val env = new MutableEnv
    val domain = variables.toSeq
    val values = domain.map(v => v.values.toStream)
    var cartesian = Util.Cartesian.cartesianProduct(values)

    for (tuple <- cartesian) {
      for (index <- 0 until domain.size) {
        env += (domain(index) -> tuple(index))
      }
      procedure(env)
    }
  }
}


case class EnvComparison(env1: Env, env2: Env) {

  //we compare composed functions by counting how often, for a given value v in the range,
  //both env1 and env2 map the same sequence of arguments (composed) to v
  //this amounts to true positives (v=true) and true negatives (v=false) for functions
  //mapping to the Bools (i.e. V1->V2->...->Bools)
  //if values are of atomic type T, we consider them of type T->{Nothing}

  def overlap[T, V](funVar: Var[T => V], value: V): Int = {
    //this is probably the slowest implementation possible
    var functionValues = funVar.values.asInstanceOf[FunctionValues[T, V]]
    var count = 0
    for (arg <- functionValues.domain) {
      val v1 = env1.resolveVar(FunAppVar(funVar, arg))
      val v2 = env2.resolveVar(FunAppVar(funVar, arg))
      if (v1 == value && v1 == v2) count += 1
    }
    count
  }

}



class OverlayedEnv(val under: Env, val over: Env) extends Env {
  def resolveVar[T](variable: EnvVar[T]) = {
    over.resolveVar(variable) match {
      case Some(x) => Some(x)
      case None => under.resolveVar(variable)
    }
  }


  def variables = over.variables ++ under.variables
}

class MaskedEnv(var unmasked: Env, var hiddenVariables: Set[EnvVar[_]]) extends Env {
  def resolveVar[T](variable: EnvVar[T]) = {
    if (hiddenVariables.contains(variable)) None else unmasked.resolveVar(variable)
  }

  //todo: this doesn't work if hiddenVariables contain funapp vars
  def variables = unmasked.variables -- hiddenVariables
}

class MutableEnv extends Env {

  private type MapType = HashMap[Any,Any]

  private var values = new MapType
  private var closed = new HashSet[EnvVar[_]]

  def resolveVar[T](variable: EnvVar[T]) = {
    var result = variable match {
      case v: Var[_] => values.get(variable).asInstanceOf[Option[T]]
      case FunAppVar(funVar, arg) => getMap(funVar).get(arg).asInstanceOf[Option[T]]
    }
    if (closed.contains(variable)) convertToClosed(variable, result) else result
  }

  def convertToClosed[T](variable: EnvVar[T], value: Option[T]): Option[T] = {
    value match {
      case Some(x: MutableFunctionValue[_, _]) => Some(x.close.asInstanceOf[T])
      case Some(_) => value
      case None => Some(variable.values.defaultValue)
    }
  }

  private def getMap(variable: EnvVar[Any]): MutableFunctionValue[Any,Any] = {
    val signature = variable.values.asInstanceOf[FunctionValues[Any,Any]]
    variable match {
      case v: Var[_] => values.getOrElseUpdate(v, new MutableFunctionValue(signature)).
              asInstanceOf[MutableFunctionValue[Any,Any]]
      case FunAppVar(funVar, arg) => getMap(funVar).getOrElseUpdate(arg, new MutableFunctionValue(signature)).
              asInstanceOf[MutableFunctionValue[Any, Any]]
    }
  }


  override def clone = {
    val result = new MutableEnv
    result.values = cloneMutableMap(values)
    result
  }

  def close(variable: EnvVar[_], closed: Boolean) : Unit = {
    if (closed) this.closed += variable else this.closed.removeEntry(variable)
  }

  def close(variables:Iterable[EnvVar[_]],closed:Boolean): Unit = variables.foreach(close(_,closed))


  private def cloneMutableMap(map: MapType): MapType = {
    val result = new MapType
    map foreach {
      case (key, value) =>
        if (value.isInstanceOf[MutableFunctionValue[_,_]])
          result += (key -> value.asInstanceOf[MutableFunctionValue[_,_]].clone)
        else
          result += (key -> value)
    }
    result
  }

  def set[T](variable: EnvVar[T], value: T) {
    variable match {
      case v: Var[_] => values += Tuple2[Any, Any](v, value)
      case FunAppVar(funVar, arg) =>
        getMap(funVar)(arg)= value
    }
  }

  def update[T,R](funVar: FunctionVar[T,R], arg: T, value:R) = {
    set(FunAppVar(funVar,arg),value)
  }

  def update[T,R](groundFunApp:GroundFunApp[T,R],value:R) = {
    set(FunAppVar(groundFunApp.funVar,groundFunApp.arg),value)
  }


  def +=[T](funVar: FunctionVar[T,Boolean], arg: T) = {
    set(FunAppVar(funVar,arg),true)
  }
  
  def ++=[T](funVar: FunctionVar[T,Boolean], args: T*) = {
    for (arg <- args)
      set(FunAppVar(funVar,arg),true)
  }

  def atoms[T](funVar:FunctionVar[T,Boolean]) = new {
    def +=(arg:T) = set(FunAppVar(funVar,arg),true)  
    def ++=(args:T*) = args.foreach(arg=>set(FunAppVar(funVar,arg),true))  
    def ++=(args:Iterable[T]) = args.foreach(arg=>set(FunAppVar(funVar,arg),true))  
  }

  def update[T](variable: EnvVar[T], value: T) = {
    set(variable,value)
  }

  def +=[T](mapping: Tuple2[EnvVar[T], T]) = set(mapping._1, mapping._2)

  def mapTo[T](envVar: EnvVar[T]) = new VarWithEnv(envVar, this)

  def variables = values.keySet.asInstanceOf[Set[EnvVar[_]]]

  override def toString = values.toString
}

case class GroundFunApp[T,R](funVar:FunctionVar[T,R], arg:T)

case class VarWithEnv[T](envVar: EnvVar[T], env: MutableEnv) {
  def ->(t: T) = env.set(envVar, t)
}

case class MapToBuilder[T, R](val funVar: EnvVar[T => R], val env: MutableEnv) {
  def apply(t: T) = VarWithEnv(FunAppVar(funVar, t), env)
}