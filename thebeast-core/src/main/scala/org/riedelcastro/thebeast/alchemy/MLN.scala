package org.riedelcastro.thebeast.alchemy


import collection.mutable.{ArrayBuffer, HashMap}
import org.riedelcastro.thebeast._
import env._
import booleans.{BooleanFunApp, BooleanTerm}
import doubles.{AlchemyIndicator}
import java.io.{Reader}
import tuples.{TupleValues, TupleTerm, TupleValues3, TupleValues2}
import vectors._

/**
 * @author Sebastian Riedel
 */

class MLN {
  import TheBeastImplicits._
  import AlchemyParser._


  /**
   * This loads a set of atoms from the given reader. Note that this will modify
   * the types associated with the predicates mentioned in the reader/file. Also note
   * that before atoms can be loaded an MLN with the mentioned
   * predicates has to be loaded.
   */
  def loadAtoms(reader: Reader): MutableEnv = {
    //this loads atoms from a database file and updates/adds types, predicates etc.
    val atoms = AlchemyParser.parse(AlchemyParser.database, reader) match {
      case Success(expr,_) => expr
      case x => error("Can't parse:" + x)
    }
    val env = new MutableEnv
    for (atom <- atoms) atom match {
      case DatabaseAtom(name,args,state) => {
        val predicate = getPredicate(name)
        env(predicate,toTuple(args.map(toValue(_)))) = state
        //env.mapTo(predicate)(toTuple(args.map(toValue(_)))) -> state
      }
    }
    env
  }

  private def toValue(any:Any) = any match {
    case Constant(x) => x
    case _ => any
  }

  private def toTuple(args:Seq[Any]) = args.size match {
    case 1 => args(0)
    case 2 => (args(0),args(1))
    case 3 => (args(0),args(1),args(2))
    case _ => error("Can't do arity " + args.size + " yet")
  }


  def loadMLN(reader: Reader) = {
    val expressions = AlchemyParser.parse(AlchemyParser.mln, reader) match {
      case AlchemyParser.Success(expr, _) => expr
      case _ => null
    }
    for (expr <- expressions) expr match {
      case AlchemyParser.ConstantTypeDefinition(typeName, constants) => {
        val newType = new MutableValues[String]
        newType ++= constants.elements
        values(typeName) = newType
      }
      case AlchemyParser.IntegerTypeDefinition(typeName, from, to) => {
        values(typeName) = new IntRangeValues(from, to)
      }
      case AlchemyParser.Atom(predName, args) => {
        predicates.get(predName) match {
          case Some(Predicate(varName, values)) =>
          case None => {
            val types = new ArrayBuffer[Values[_]]
            for (arg <- args) arg match {
              case AlchemyParser.VariableOrType(typeName) => {
                types += getType(typeName)
              }
              case AlchemyParser.ExclamationType(typeName) => {
                types += getType(typeName)
                //add uniqueness constraint
              }
            }
            val predicate:Predicate[Any] = types.size match {
              case 0 => error("Can't do 0 arguments")
              case 1 => Predicate(predName, types(0))
              case 2 => Predicate(predName, TupleValues2(types(0), types(1)))
              case 3 => Predicate(predName, TupleValues3(types(0), types(1), types(2)))
              case _ => error("Can't do more than 3 arguments yet")
            }
            predicates(predName) = predicate
          }
        }
      }
      case AlchemyParser.WeightedFormula(weight, formula) => {
        addFormula(formula, weight)
      }
      case f: AlchemyParser.Formula => {
        addFormula(f, 0.0)
      }
      case _ => error("Don't know how to handle " + expr)

    }
  }


  private def boundVariables(formula: Formula): Set[Variable] = {
    formula match {
      case f => {
        f.subformulas.foldLeft(Set[Variable]()) {(r, s) => r ++ boundVariables(s)}
      }
    }
  }

  private def createVectorSum(formula: VectorTerm, variables: Set[Var[Any]]): QuantifiedVectorSum[Any] = {
    if (variables.size == 0) error("No variable, can't quantify")
    val first = variables.elements.next
    if (variables.size == 1) QuantifiedVectorSum(first, formula)
    else QuantifiedVectorSum(first, createVectorSum(formula, variables - first))
  }



  private class BoolTermBuilder {
    private val name2var = new HashMap[String, Var[Any]]

    def convertArgs(predName: String, args: List[Term]): Seq[env.Term[Any]] = {
      val domain = getPredicate(predName).values.asInstanceOf[FunctionValues[Any, Boolean]].domain
      args.size match {
        case 0 => error("Can't have zero arguments")
        case 1 => Seq(convertTerm(args(0), domain))
        case _ => for (i <- 0 until args.size) yield
          convertTerm(args(i), domain.asInstanceOf[TupleValues[Any]].productElement(i).asInstanceOf[Values[Any]])
      }
    }

    def build(formula: Formula): BooleanTerm = {
      formula match {
        case Atom(name, args) => BooleanFunApp(getPredicate(name), TupleTerm(convertArgs(name, args):_*))
        case And(lhs, rhs) => env.booleans.AndApp(build(lhs), build(rhs))
        case Implies(lhs, rhs) => env.booleans.ImpliesApp(build(lhs), build(rhs))
        case Equivalence(lhs, rhs) => env.booleans.EquivalenceApp(build(lhs), build(rhs))
        case Not(arg) => env.booleans.NotApp(build(arg))
        case _ => error("We don't support a " + formula + " formula yet")
      }
    }

    def convertTerm(term: Term, values: Values[Any]): env.Term[Any] = {
      term match {
        case Constant(text) => env.Constant(text)
        case PlusVariable(name) => name2var.getOrElseUpdate(name, Var(name, values))
        case VariableOrType(name) => name2var.getOrElseUpdate(name, Var(name, values))
        case _ => error("we don't support a " + term + " term yet")
      }
    }

    def convertKnownVar(variable: Variable) = name2var(variable.name)

    def convertKnownVars(variables: Iterable[Variable]): List[Var[Any]] = variables.map(convertKnownVar(_)).toList

  }

  private def getVariables(alchemyVars: Set[Variable]): Set[Var[Any]] = null

  private def addFormula(formula: Formula, weight: Double): VectorTerm = {
    //the id of this formula
    val id = formulaeIds.getOrElseUpdate(formula, "F" + formulaeIds.size)
    //set init value for weight
    weights.setDefaultForFirstKey(id,weight)
    //formula builder
    val builder = new BoolTermBuilder
    //build boolean formula
    val bool = builder.build(formula)
    //creating mapping to real values according to AlchemyIndicator
    val indicator = AlchemyIndicator(bool)
    //multiply with unit vector indexed with formula id and plus variables
    val vectorTerm = VectorScalarApp(
      UnitVector((env.Constant(id) :: builder.convertKnownVars(formula.allPlusVariables): _*)),
      indicator)
    //now we find bound variables
    val bound = boundVariables(formula)
    //then we need to find all variables
    val all = formula.allVariables
    //this gives the unbound variables
    val unbound = all -- bound
    //we create a quantified vector sum with unbound variables
    val converted = if (unbound.size > 0) {
      createVectorSum(vectorTerm, Set(builder.convertKnownVars(unbound): _*))
    } else
      vectorTerm
    //add converted formula
    formulae += converted
    //return
    converted
  }

  def getType(typeName: String): Values[_] = {
    values.getOrElseUpdate(typeName, new MutableValues)
  }

  def getPredicate(name: String): Predicate[Any] = predicates(name)

  def getFormula(index:Int) = formulae(index)

  def getFormulae:Seq[VectorTerm] = formulae

  def getWeights = weights

  private val values = new HashMap[String, Values[_]]
  private val formulaeIds = new HashMap[Formula, String]
  private val predicates = new HashMap[String, Predicate[Any]]
  private val formulae = new ArrayBuffer[VectorTerm]
  private val weights = new Vector

}