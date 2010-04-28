package org.riedelcastro.thebeast.env.vectors


import org.riedelcastro.thebeast.env._
import collection.mutable.ArrayBuffer
import doubles.{DoubleFunApp, Sum, QuantifiedSum, DoubleTerm}
import java.lang.String

/**
 * @author Sebastian Riedel
 */

trait VectorTerm extends Term[Vector] with NumericTerm[Vector] {
  def ground(env: Env) : VectorTerm

  def *(that:DoubleTerm) = VectorScalarApp(this,that)

  def +(that:VectorTerm) = {
    this match {
      case VectorAddApp(lhs,rhs) => that match {
        case VectorAddApp(lhs2,rhs2) => VectorSum(Seq(lhs,rhs,lhs2,rhs2))
        case VectorSum(args) => VectorSum(Seq(lhs,rhs) ++ args)
        case x => VectorSum(Seq(lhs,rhs,x))
      }
      case VectorSum(args) => that match {
        case VectorAddApp(lhs2,rhs2) => VectorSum(args ++ Seq(lhs2,rhs2))
        case VectorSum(args2) => VectorSum(args ++ args2)
        case x => VectorSum(args ++ Seq(x))
      }
      case _ => VectorSum(Seq(this,that))
    }

  }
  def dot(that:VectorTerm) = VectorDotApp(this,that)

  def dot(that:Vector) = VectorDotApp(this,VectorConstant(that))

  def flatten: VectorTerm = this


  def createExpectation = new Expectation[Vector] {
    val value = new Vector
    def add(prob: Double, t: Vector) = value.addInPlace(t,prob)
    override def toString = value.toString
  }
}

case class UnitVector(key : Term[Any]*) extends VectorTerm {

  def ground(env: Env) = UnitVector(key.map(k => k.ground(env)):_*)

  def simplify = {
    if (!key.exists(k => !k.isInstanceOf[Constant[_]])) {
      val result = new Vector
      result.set(1.0, key.map(k => k.asInstanceOf[Constant[Any]].value):_*)
      VectorConstant(result)
    } else
      this
  }

  override def eval(env: Env): Option[Vector] = {
    val keyEvals = new ArrayBuffer[Any]
    for (k <- key) { val eval = k.eval(env); if (eval.isDefined) keyEvals += eval.get else return None }
    val result = new Vector
    result.set(1.0, keyEvals:_*)
    Some(result)
  }

  def variables = key.foldLeft(Set[EnvVar[_]]()){(set,k) => set ++ k.variables} // Set(key.flatMap(k => k.variables))

  def values = VectorSpace

  override def toString = "1_(" + key.mkString(",") + ")"

  def subterms = key.toSeq


  override def equals(obj: Any): Boolean = obj match {
    case x:UnitVector => x.key.size == this.key.size && (0 until key.size).forall(i=> this.key(i) == x.key(i))
    case _ => false
  }
}

case class VectorAddApp(lhs:VectorTerm, rhs:VectorTerm)
        extends FunApp(FunApp(Constant(VectorAdd),lhs),rhs) with VectorTerm {
  override def ground(env: Env) = VectorAddApp(lhs.ground(env),rhs.ground(env))


  override def toString = lhs + "+" + rhs
}

case class VectorVar(override val name: String)
        extends Var[Vector](name, VectorValues) with VectorTerm {
  override def ground(env: Env) = env.eval(this).map(new VectorConstant(_) with GroundedConstant).getOrElse(this)

  override def simplify = this
}


case class VectorDotApp(lhs:VectorTerm, rhs:VectorTerm)
        extends DoubleFunApp(FunApp(Constant(VectorDot),lhs),rhs) {
  override def ground(env: Env) = VectorDotApp(lhs.ground(env),rhs.ground(env))

  def distribute : DoubleTerm = {
    lhs match {
      case VectorSum(args) => Sum(args.map(a => (a dot rhs).distribute))
      case QuantifiedVectorSum(variable,formula) => QuantifiedSum(variable, (formula dot rhs).distribute)
      case _ => rhs match {
        case VectorSum(args) => Sum(args.map(a => (lhs dot a).distribute))
        case QuantifiedVectorSum(variable,formula) => QuantifiedSum(variable, (lhs dot formula).distribute)
        case _ => this
      }
    }
  }


  override def flatten: DoubleTerm = VectorDotApp(lhs.flatten,rhs.flatten)

  override def toString = "(%s)^T * (%s)".format(lhs,rhs)

  override def equals(obj: Any): Boolean = obj match {
    case VectorDotApp(lhs,rhs) => this.lhs == lhs && this.rhs == rhs
    case _ => false
  }
}

case class VectorScalarApp(lhs:VectorTerm, rhs:DoubleTerm)
        extends FunApp(FunApp(Constant(VectorScalar),lhs),rhs) with VectorTerm {
  override def ground(env: Env) = VectorScalarApp(lhs.ground(env),rhs.ground(env))

  def upperBound = Math.POS_INF_DOUBLE


  override def toString: String = "(%s) * (%s)".format(lhs,rhs)

}

case class VectorConstant(override val value:Vector) extends Constant(value) with VectorTerm {
  override def ground(env: Env) = this
}

case class VectorSum(override val args:Seq[VectorTerm])
        extends Fold(Constant(VectorAdd),args,Constant(new Vector)) with VectorTerm {
  override def eval(env: Env) : Option[Vector] = {
    val result = new Vector;
    for (a <- args) {
      val eval = a.eval(env)
      if (eval.isDefined) result.addInPlace(eval.get,1.0) else return None
    }
    Some(result)
  }

  override def ground(env: Env) = VectorSum(args.map(a=>a.ground(env)))


  override def toString = if (args.size != 2) "+." + args.mkString(",") else "(%s) .+ (%s)".format(args(0),args(1))


  override def flatten: VectorSum =
    VectorSum(args.flatMap(a => a match {case x: VectorSum => x.flatten.args; case _ => Seq(a)}))


  override def equals(obj: Any): Boolean = obj match {
    case x:VectorSum => (0 until args.size).forall(i=>args(i) == x.args(i))
  }
}

case class QuantifiedVectorSum[T](override val variable: Var[T], override val formula: VectorTerm)
        extends Quantification(Constant(VectorAdd), variable, formula, Constant(new Vector)) with VectorTerm {
  override lazy val unroll = {
    val env = new MutableEnv
    VectorSum(variable.values.map(value => {env += variable -> value; formula.ground(env)}).toSeq).flatten
  }

  override def unrollUncertain = VectorSum(unroll.args.filter(arg => !arg.simplify.isGround))


  override def ground(env: Env) = QuantifiedVectorSum(variable,formula.ground(env.mask(Set(variable))))


  override def toString = "vSum(" + variable + "," + formula + ")"
}
