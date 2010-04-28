package org.riedelcastro.thebeast.env.booleans

import org.riedelcastro.thebeast.env._
import doubles.Indicator
import functions._
import org.riedelcastro.thebeast.util.{Util, SimpleNamed}
import java.lang.String

/**
 * @author Sebastian Riedel
 */

trait BooleanTerm extends BoundedTerm[Boolean] {
  def @@ = Indicator(this)

  def &&(rhs: BooleanTerm) = AndApp(this, rhs)

  def ~>(rhs: BooleanTerm) = ImpliesApp(this, rhs)

  def <~>(rhs: BooleanTerm) = EquivalenceApp(this, rhs)

  def unary_! = negate

  def ground(env: Env): BooleanTerm


  def simplify: BooleanTerm

  lazy val toCNF: CNF = moveInNegation.distributeAnds.flatten match {
    case x: BooleanConstant => CNF(Seq(Disjunction(Seq(x))))
    case Conjunction(args) => CNF(args.map(a => a match {
      case x: Disjunction[_] => x.flatten
      case x => Disjunction(Seq(x))
    })).trim
    case x: NotApp => CNF(Seq(Disjunction(Seq(x))))
    case x => error("After moving in negations and distributing ands the term must be a constant or conjunction" +
                    " or a literal and not a " + x)
  }

  def negate: BooleanTerm

  def moveInNegation: BooleanTerm

  def distributeAnds: BooleanTerm = this

  def flatten: BooleanTerm

}

object Bools extends Values[Boolean] {
  val seq = Seq(false, true)

  def elements = seq.elements
}

case class Conjunction[T <: BooleanTerm](override val args: Seq[T]) extends Fold[Boolean](Constant(And), args, Constant(true))
    with BooleanTerm with Composite[Boolean,Conjunction[T],T] {
  //override def ground(env: Env) = Conjunction(args.map(_.ground(env)))



  //def this(args:T*) = this((args:*).toSeq)

  def build(members: Seq[T]) = Conjunction(members)

  def members: Seq[T] = args

  def upperBound = !args.exists(!_.upperBound)

  def negate = Disjunction(args.map(_.negate))

  override def distributeAnds = Conjunction(args.map(_.distributeAnds))

  def flatten = Conjunction(args.flatMap(a => a.flatten match {case Conjunction(inner) => inner; case _ => Seq(a)}))

  def moveInNegation = Conjunction(args.map(_.moveInNegation))


  override def toString = args.mkString("(", " & ", ")")

  override def simplify:BooleanTerm = Util.mapUntil(args, (arg:T)=> arg.simplify match {
    case BooleanConstant(false) => None
    case x => Some(x)
  }).toLeft(None).fold(Conjunction(_),x => BooleanConstant(false))

  
}



case class Disjunction[+T <: BooleanTerm](override val args: Seq[T]) extends Fold[Boolean](Constant(Or), args, Constant(false))
    with BooleanTerm {
  override def ground(env: Env) = Disjunction(args.map(_.ground(env)))

  //def this(args:T*) = this((args:*).toSeq)

  def upperBound = args.exists(_.upperBound)

  def negate = Conjunction(args.map(_.negate))

  override def distributeAnds = {
    val groups = args.map(_.distributeAnds).map(a => a match {
      case Conjunction(args) => args.toStream
      case _ => Stream(a)
    })
    Conjunction(Util.Cartesian.cartesianProduct(groups).map(Disjunction(_)).toSeq)
  }

  def flatten = Disjunction(args.flatMap(a => a.flatten match {case Disjunction(inner) => inner; case _ => Seq(a)}))

  def moveInNegation = Disjunction(args.map(_.moveInNegation))

  override def toString = args.mkString("(", " | ", ")")

  override def simplify:BooleanTerm = Util.mapUntil(args, (arg:T)=> arg match {
     case BooleanConstant(true) => None
     case x => Some(x)
   }).toLeft(None).fold(Conjunction(_),x => BooleanConstant(true))



}

case class CNF(override val args: Seq[Disjunction[BooleanTerm]]) extends Conjunction(args) {
  override def ground(env: Env): CNF = CNF(args.map(_.ground(env).asInstanceOf[Disjunction[BooleanTerm]]))

  def trim = CNF(args.filter(d => !d.args.exists(x => d.args.exists(y => x == NotApp(y)))))
}

case class DNF[T <: BooleanTerm](override val args: Seq[Conjunction[T]]) extends Disjunction(args) {
  override def ground(env: Env): DNF[T] = DNF(args.map(_.ground(env).asInstanceOf[Conjunction[T]]))

}

object DNFMatch {
  def unapply(term: BooleanTerm): Option[DNF[BooleanTerm]] = {
    term.flatten match {
      case Disjunction(args@Seq(Conjunction(_))) => Some(DNF(args.asInstanceOf[Seq[Conjunction[BooleanTerm]]]))
      case _ => None
    }
  }
}



object BooleanEnvVarMatch {
  def unapply(term: Term[Boolean]): Option[BooleanEnvVar] = {
    term match {
      case x: BooleanEnvVar => Some(x)
      case FunApp(EnvVarMatch(f), Grounded(arg)) => Some(BooleanFunAppVar(f,arg))
      case _ => None
    }
  }
}

object LiteralMatch {
  def unapply(term: BooleanTerm): Option[BooleanLiteral] = {
    term match {
      case BooleanEnvVarMatch(x) => Some(x)
      case NotApp(BooleanEnvVarMatch(v)) => Some(NegatedVar(v))
      case _ => None
    }
  }
}

object LiteralDNFMatch {
  def unapply(term: BooleanTerm): Option[DNF[BooleanLiteral]] = {
    term.flatten match {
      case Disjunction(args) =>
        Util.optionalMap(args.asInstanceOf[Seq[Conjunction[_]]])(conj => Util.optionalMap(conj.args)(_ match {
          case LiteralMatch(lit) => Some(lit)
          case _ => None
        }).map(args => Conjunction(args.toSeq))).map(conj => DNF(conj.toSeq))
      case _ => None
    }
  }
}

case class Equality[+T](lhs:Term[T],rhs:Term[T]) extends BooleanTerm {
  def upperBound = true

  def subterms = Seq(lhs,rhs)

  def eval(env: Env) = lhs.eval(env).flatMap(x=> rhs.eval(env).map(y=> x == y))//for (x <- lhs.eval(env); y <- rhs.eval(env)) x == y

  def values = Bools

  def variables = lhs.variables ++ rhs.variables

  def flatten = this

  def moveInNegation = this

  def negate = NotApp(this)

  def simplify = Equality(lhs.simplify,rhs.simplify)

  def ground(env: Env) = Equality(lhs.ground(env),rhs.ground(env))
}




case class BooleanConstant(override val value: Boolean) extends BoundedConstant(value) with BooleanTerm {
  override def ground(env: Env) = this

  def negate = BooleanConstant(!value)

  override def distributeAnds = this

  def moveInNegation = this

  def flatten = this

  override def simplify = this
}

case class BooleanFunApp[T](override val function: Term[T => Boolean], override val arg: Term[T])
    extends FunApp(function, arg) with BooleanTerm {
  def upperBound = true

  //todo: this is bad, ideally this should remain empty here and in FunApp
  override def ground(env: Env): BooleanTerm = BooleanFunApp(function.ground(env), arg.ground(env))

  def negate: BooleanTerm = NotApp(this)

  def flatten = this

  def moveInNegation: BooleanTerm = this

  override def simplify: BooleanTerm =
    function.simplify match {
      case Constant(f) => arg.simplify match {
        case Constant(x) => BooleanConstant(f(x));
        case x => BooleanFunApp(Constant(f), x)
      }
      case f => BooleanFunApp(f, arg.simplify)
    }
}


case class AndApp(lhs: BooleanTerm, rhs: BooleanTerm) extends Conjunction(Seq(lhs, rhs)) {
}
case class OrApp(lhs: BooleanTerm, rhs: BooleanTerm) extends Disjunction(Seq(lhs, rhs)) {
}

case class NotApp(override val arg: BooleanTerm) extends BooleanFunApp(Constant(Not), arg) {
  override def negate = arg

  override def ground(env: Env) = NotApp(arg.ground(env))

  override def distributeAnds = NotApp(arg.distributeAnds)

  override def flatten = NotApp(arg.flatten)

  override def moveInNegation = arg.negate

  override def toString = "!" + arg

}

case class NegatedVar(val variable: BooleanEnvVar) extends NotApp(variable) with BooleanLiteral {
  def negated = true
}

trait BooleanLiteral extends BooleanTerm {
  def variable: BooleanEnvVar

  def negated: Boolean
}

trait BooleanEnvVar extends BooleanTerm with EnvVar[Boolean] with BooleanLiteral {
  override def ground(env: Env) = env.eval(this).map(BooleanConstant(_)).getOrElse(this)

  override def simplify = this

  def negated = false

  def variable = this

  def flatten = this

  def moveInNegation = this

  def upperBound = true

  def negate = NegatedVar(this)
}

case class BooleanVar(override val name: String)
    extends Var[Boolean](name, Bools) with BooleanEnvVar {
  override def simplify = this
}

case class BooleanFunAppVar[T](override val funVar: EnvVar[T => Boolean], override val argValue: T)
    extends FunAppVar(funVar,argValue) with BooleanEnvVar {
  override def simplify = this

}

case class Forall[T](override val variable: Var[T], override val formula: BooleanTerm)
        extends Quantification(Constant(And), variable, formula, Constant(true)) with BooleanTerm {
  override lazy val unroll = {
    val env = new MutableEnv
    Conjunction(variable.values.map(value => {env += variable -> value; formula.ground(env)}).toSeq)
  }

  override def unrollUncertain = Conjunction(unroll.args.filter(arg => !arg.simplify.isGround))

  def upperBound = unroll.upperBound

  override def ground(env: Env) = Forall(variable,formula.ground(env.mask(Set(variable))))

  override def simplify = Forall(variable,formula.simplify)

  def flatten = this

  def moveInNegation: BooleanTerm = this

  def negate: BooleanTerm = NotApp(this)


  override def toString: String = "{forall %s: %s}".format(variable,formula)
}


case class ImpliesApp(lhs: BooleanTerm, rhs: BooleanTerm) extends Disjunction(Seq(NotApp(lhs), rhs)) {
  override def toString = lhs + "=>" + rhs
}

case class EquivalenceApp(lhs: BooleanTerm, rhs: BooleanTerm)
    extends Disjunction(Seq(Conjunction(Seq(lhs, rhs)), Conjunction(Seq(NotApp(lhs), NotApp(rhs))))) {
  override def toString = lhs + "<=>" + rhs

}

trait BooleanBinaryOperator extends (Boolean => (Boolean => Boolean)) with SimpleNamed

trait BooleanUnaryOperator extends (Boolean => Boolean) with SimpleNamed

object And extends BooleanBinaryOperator {
  def apply(arg1: Boolean): (Boolean => Boolean) = (arg2: Boolean) => arg1 && arg2
}

object Or extends BooleanBinaryOperator {
  def apply(arg1: Boolean): (Boolean => Boolean) = (arg2: Boolean) => arg1 || arg2
}

object Implies extends BooleanBinaryOperator {
  def apply(arg1: Boolean): (Boolean => Boolean) = (arg2: Boolean) => !arg1 || arg2
}

object Not extends BooleanUnaryOperator {
  def apply(arg1: Boolean): Boolean = !arg1
}

object Equivalence extends BooleanBinaryOperator {
  def apply(arg1: Boolean): (Boolean => Boolean) = (arg2: Boolean) => arg1 == arg2
}

