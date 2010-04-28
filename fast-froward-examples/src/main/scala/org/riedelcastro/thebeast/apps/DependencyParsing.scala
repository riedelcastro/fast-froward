package org.riedelcastro.thebeast.apps

import org.riedelcastro.thebeast.env._
import org.riedelcastro.thebeast.util._
import combinatorics.SpanningTreeConstraint
import vectors.{Vector, VectorVar}
import org.riedelcastro.thebeast.solve.SumProductBeliefPropagation
import org.riedelcastro.thebeast.env.TheBeastImplicits._
import io.Source
import collection.mutable.{HashMap, ArrayBuffer}

/**
 * Simple Dependency Parsing model
 */
object DependencyParsing extends TheBeastEnv {

  val maxLength = 50

  val Tokens = Ints(0 until maxLength)
  val Words = new MutableValues[String]()
  val Tags = new MutableValues[String]()
  val length = Var("length", Ints(1 until maxLength))
  val link = Predicate("link", Tokens x Tokens)
  val word = Predicate("word", Tokens x Words)
  val pos = Predicate("pos", Tokens x Tags)
  val token = Predicate("token", Tokens)
  val candidate = Predicate("cand", Tokens x Tokens)

  val ROOT = "Root"

  def asTokenProperties[T](seq:Seq[T]):Seq[(Int,T)]  = {
    for (i <- 0 until seq.size) yield i -> seq(i)
  }

  def asTokenProperties[T](root:T, seq:Seq[T]):Seq[(Int,T)]  = {
    Seq((0,root)) ++ (for (i <- 0 until seq.size) yield i + 1 -> seq(i))
  }


  def loadCoNLLFile(file: String, from:Int, to:Int): Seq[Env] = {
    val result = new ArrayBuffer[Env]
    val rows = new ArrayBuffer[Array[String]]
    for (line <- Source.fromFile(file).getLines.map(_.trim)) {
      if ("" == line) {
        val env = new MutableEnv
        env.atoms(token) ++= Seq(0) ++ rows.map(row => row(0).toInt)
        env.atoms(word) ++= asTokenProperties(ROOT,rows.map(row => row(1)))
        env.atoms(pos) ++= asTokenProperties(ROOT,rows.map(row => row(3)))
        env.atoms(link) ++= rows.map(row => row(6).toInt->row(0).toInt)
        for (i <- 0 until rows.size + 1; j <- 1 until rows.size+1; if (i != j)){
          env.atoms(candidate) ++= Seq(i->j)
        }
        env.close(Set(token,word,pos,link,candidate),true)
        result += env
        if (result.size == to) return result.drop(from)
      } else {
        rows += line.split("\\s+")
      }
    }
    result
  }

  def trainNaively(data:Seq[Env]) : Map[(String,String),Double] = {
    val childCounts = new HashMap[String,Int]
    val edgeCounts = new HashMap[(String,String),Int]
    for (env:Env <- data){
      val tags = Map() ++ env(pos).getSources(Some(true))
      for (edge <- env(link).getSources(Some(true))){
        val childTag = tags(edge._2)
        var headTag = tags(edge._1)
        childCounts(childTag) = childCounts.getOrElse(childTag,0) + 1
        edgeCounts(headTag->childTag) = edgeCounts.getOrElse(headTag->childTag,0) + 1
      }
    }
    Map() ++ (for (edge <- edgeCounts.keys) yield edge -> Math.log(edgeCounts(edge).toDouble / childCounts(edge._2)))
  }


  def main(args: Array[String]): Unit = {

    Logging.level = Logging.DEBUG




    //first order formulae
    val bias = vectorSum(Tokens, Tokens) {
      (h, m) =>
        $(link(h, m)) * unit("bias")
    }
    val wordPair = vectorSum(Tokens, Tokens, Words, Words) {
      (h, m, h_word, m_word) =>
        $(word(h, h_word) && word(m, m_word) && link(h, m)) * unit(h_word, m_word)
    }
    val posPair = vectorSum(Tokens, Tokens, Tags, Tags) {
      (h, m, h_pos, m_pos) =>
        $(pos(h, h_pos) && pos(m, m_pos) && link(h, m)) * unit(h_pos, m_pos)
    }

    val treeConstraint = SpanningTreeConstraint(link, token, 0, LessThan(Tokens))

    val weightVar = VectorVar("weights")
    //val linearModel = ((wordPair + posPair + bias) dot weightVar) + treeConstraint
    val linearModel = ((wordPair + posPair + bias) dot weightVar)
    val probModel = normalize(exp(linearModel) * ptree(link, token, 0, LessThan(Tokens)))

    //some example data
    val sentence1 = new MutableEnv
    sentence1(length) = 5
    sentence1.atoms(word) ++= List("Root", "The", "man", "is", "fast").zipWithIndex.map(_.swap)
    sentence1.atoms(pos) ++= List("Root", "DT", "NN", "VB", "AD").zipWithIndex.map(_.swap)
    sentence1.atoms(link) ++= List((0, 3), (3, 2), (3, 4), (2, 1))
    sentence1.atoms(token) ++= (0 until 5)
    sentence1.close(word, true)
    sentence1.close(pos, true)
    sentence1.close(link, true)
    sentence1.close(token, true)

    val weights = new Vector
    weights("bias") = -2.0
    weights("NN", "DT") = 1.0
    weights("VB", "NN") = 1.0
    weights("Root", "VB") = 1.0
    weights("VB", "AD") = 1.0

    sentence1(weightVar) = weights

    println(Words.mkString(","))
    println(Tags.mkString(","))

    println(sentence1(linearModel))

    //run inference
    val bp = new SumProductBeliefPropagation
    val marginals = bp.infer(probModel.ground(sentence1.mask(Set(link))))


    println(marginals)

    var sum = 0.0
    for (h <- 0 until 5; if (h != 1)) {
      sum += marginals.belief(FunAppVar(link, (h, 1))).belief(true)
    }
    println(sum)
    println(ptree(link, token, 0, LessThan(Tokens)).asLogic)

  }

}

object NaiveDependencyParsingApp {

  import DependencyParsing._

  def main(args:Array[String]) = {

    Logging.level = Logging.DEBUG
    
    val trainData = loadCoNLLFile(args(0),0,10)
    println(trainData(0)(word))
    println(trainData(0)(pos))
    println(trainData(0)(link))

    val posPairProbs = trainNaively(trainData)
    println(posPairProbs)

    val posPair = vectorSum(Tokens, Tokens, Tags, Tags) {
      (h, m, h_pos, m_pos) =>
        $(pos(h, h_pos) && pos(m, m_pos) && candidate(h,m) && link(h, m)) * unit("Pos", h_pos, m_pos)
    }

    val treeConstraint = SpanningTreeConstraint(link, token, 0, LessThan(Tokens))

    val theta = VectorVar("theta")
    //val linearModel = ((wordPair + posPair + bias) dot weightVar) + treeConstraint
    val linearModel = (posPair dot theta)
    val probModel = normalize(exp(linearModel) * ptree(link, token, 0, LessThan(Tokens)))
    val mlModel = normalize(exp(linearModel) * ptree(link, token, 0, LessThan(Tokens)).asLogic)

    val weights = new Vector
    for (pair <- posPairProbs) weights("Pos",pair._1._1,pair._1._2) = pair._2

    val global = new MutableEnv
    global(theta) = weights

    println(weights)

    val bp = new SumProductBeliefPropagation
    val marginals = bp.infer(probModel.ground(trainData(0).overlay(global).mask(Set(link))))

    println(marginals)
    
    val mlMarginals = bp.infer(mlModel.ground(trainData(0).overlay(global).mask(Set(link))))

    println(mlMarginals)

  }

}