package org.riedelcastro.thebeast.alchemy


import scala.util.parsing.combinator.{RegexParsers, JavaTokenParsers}
import collection.mutable.{HashSet, HashMap}
import org.riedelcastro.thebeast.env.tuples.{TupleTerm}
/**
 * @author Sebastian Riedel
 */

object AlchemyParser extends JavaTokenParsers with RegexParsers {
  val LowerCaseID = """[a-z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""" r
  val UpperCaseID = """[A-Z]([a-zA-Z0-9]|_[a-zA-Z0-9])*""" r
  val NumDouble = "-?\\d+(\\.\\d+)?" r
  val NumPosInt = "\\d+"
  val StringLit = "(\\w)*"


  val multiline = "(/\\*(?:.|[\\n\\r])*?\\*/)"

  override val whiteSpace = """(\s|//.+\n|(/\*(?:.|[\n\r])*?\*/))+""" r

  def constantTypeDefinition: Parser[ConstantTypeDefinition] =
    (LowerCaseID ~ "=" ~ "{" ~ repsep(UpperCaseID, ",") ~ "}") ^^
            {case name ~ "=" ~ "{" ~ constants ~ "}" => ConstantTypeDefinition(name, constants)}


  def integerTypeDefinition: Parser[IntegerTypeDefinition] =
    (LowerCaseID ~ "=" ~ "{" ~ NumPosInt ~ "," ~ "..." ~ "," ~ NumPosInt ~ "}") ^^
            {case name ~ "=" ~ "{" ~ from ~ "," ~ "..." ~ "," ~ to ~ "}" => IntegerTypeDefinition(name, from.toInt, to.toInt)}

  def include: Parser[Include] = ("#include" ~> stringLiteral) ^^ {s => Include(s)}

  def mln: Parser[List[Expression]] = rep(expression)

  def atom: Parser[Atom] = UpperCaseID ~ "(" ~ termList ~ ")" ^^ {case s ~ "(" ~ terms ~ ")" => Atom(s, terms)}

  def positiveDatabaseAtom: Parser[DatabaseAtom] = UpperCaseID ~ "(" ~ groundedTermList ~ ")" ^^
          {case s ~ "(" ~ terms ~ ")" => DatabaseAtom(s, terms, true)}

  def negativeDatabaseAtom: Parser[DatabaseAtom] = "!" ~ UpperCaseID ~ "(" ~ groundedTermList ~ ")" ^^
          {case _ ~ s ~ "(" ~ terms ~ ")" => DatabaseAtom(s, terms, false)}

  def databaseAtom:Parser[DatabaseAtom] = (positiveDatabaseAtom | negativeDatabaseAtom) ^^ {t=>t}

  def database: Parser[List[DatabaseAtom]] = rep(databaseAtom) ^^ {case t => t}

  def and: Parser[And] = (atom ~ "^" ~ formula) ^^ {case lhs ~ "^" ~ rhs => And(lhs, rhs)}

  def formula: Parser[Formula] = (binary(minPrec) | negated | atomic)

  def negated: Parser[Not] = "!" ~ atomic ^^ {case _ ~ f => Not(f)}

  def expression: Parser[Expression] =
    (weightedFormula | formula | integerTypeDefinition | constantTypeDefinition | include)

  def atomic: Parser[Formula] = (parens | atom | negated)

  def weightedFormula: Parser[WeightedFormula] =
    (NumDouble ~ formula) ^^ {case weight ~ formula => WeightedFormula(weight.toDouble, formula)}

  def termList: Parser[List[Term]] = repsep(term, ",") ^^ {case t => t}

  def groundedTermList: Parser[List[Term]] = repsep(groundedTerm, ",") ^^ {case t => t}


  def term: Parser[Term] = (variable | constant | exclType | plusVariable)

  def groundedTerm: Parser[Term] = (constant)


  def variable: Parser[VariableOrType] = LowerCaseID ^^ {s => VariableOrType(s)}

  def constant: Parser[Constant] = UpperCaseID ^^ {s => Constant(s)}

  def exclType: Parser[ExclamationType] = "!" ~> LowerCaseID ^^ {s => ExclamationType(s)}

  def plusVariable: Parser[PlusVariable] = "+" ~> LowerCaseID ^^ {s => PlusVariable(s)}

  def parens: Parser[Formula] = "(" ~> formula <~ ")"

  def deterministic: Parser[WeightedFormula] = formula <~ "." ^^ {f => WeightedFormula(Math.POS_INF_DOUBLE, f)}

  def binaryOp(level: Int): Parser[((Formula, Formula) => Formula)] = {
    level match {
      case 1 =>
        "v" ^^^ {(a: Formula, b: Formula) => Or(a, b)}
      case 2 =>
        "=>" ^^^ {(a: Formula, b: Formula) => Implies(a, b)} |
                "<=>" ^^^ {(a: Formula, b: Formula) => Equivalence(a, b)}
      case 3 =>
        "^" ^^^ {(a: Formula, b: Formula) => And(a, b)}
      case _ => throw new RuntimeException("bad precedence level " + level)
    }
  }

  val minPrec = 1
  val maxPrec = 3

  def binary(level: Int): Parser[Formula] =
    if (level > maxPrec) atomic
    else binary(level + 1) * binaryOp(level)


  def test(test: String) = {
    println(parse(expression, test))
    //println(AlchemyParser.formula(new scala.util.parsing.combinator.lexical.Scanner(test)))    
  }

  trait Expression
  trait Term extends Expression {
    def subterms: Seq[Term] = Seq()

    lazy val allVariables: Set[Variable] = this match {
      case v: Variable => Set(v)
      case _ => subterms.foldLeft(Set[Variable]()) {(r, s) => r ++ s.allVariables}
    }
    lazy val allPlusVariables: Seq[PlusVariable] =
    allVariables.filter(_.isInstanceOf[PlusVariable]).map(_.asInstanceOf[PlusVariable]).toSeq
  }
  trait Variable extends Term {
    def name: String
  }
  case class Constant(value: String) extends Term
  case class VariableOrType(name: String) extends Variable
  case class ExclamationType(name: String) extends Term
  case class PlusVariable(name: String) extends Variable

  sealed trait Formula extends Expression {
    def subformulas: Seq[Formula] = Seq()

    lazy val allVariables: Set[Variable] = this match {
      case Atom(_, args) => args.foldLeft(Set[Variable]())(_ ++ _.allVariables)
      case _ => this.subformulas.foldLeft(Set[Variable]()) {_ ++ _.allVariables}
    }
    lazy val allPlusVariables =
    allVariables.filter(_.isInstanceOf[PlusVariable]).map(_.asInstanceOf[PlusVariable]).toSeq

  }
  case class WeightedFormula(weight: Double, formula: Formula) extends Formula {
    override def subformulas = Seq(formula)
  }
  case class Atom(predicate: String, args: List[Term]) extends Formula
  case class DatabaseAtom(predicate: String, args: List[Term], positive:Boolean)

  case class Not(arg:Formula) extends Formula {
    override def subformulas = Seq(arg)
  }

  case class And(lhs: Formula, rhs: Formula) extends Formula {
    override def subformulas = Seq(lhs, rhs)
  }
  case class Or(lhs: Formula, rhs: Formula) extends Formula {
    override def subformulas = Seq(lhs, rhs)
  }
  case class Implies(lhs: Formula, rhs: Formula) extends Formula {
    override def subformulas = Seq(lhs, rhs)

  }
  case class Equivalence(lhs: Formula, rhs: Formula) extends Formula {
    override def subformulas = Seq(lhs, rhs)
  }

  case class IntegerTypeDefinition(name: String, from: Int, to: Int) extends Expression
  case class ConstantTypeDefinition(name: String, constants: Seq[String]) extends Expression

  case class Include(fileName: String) extends Expression

}


object Test extends Application {
  val test = "10.0 Same(+hallo,!po) /* Hallo\nDu Igel */ ^ \n (Popel(du,igel)) => Same(du, nuss)"

  AlchemyParser.test(test)
  AlchemyParser.test("#include \"Blah.mln\"")

}

