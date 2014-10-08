package com.volhovm.mathlogic.propositional

import Proofs._

/**
 * @author volhovm
 *         Created on 10/7/14
 */

// self-documenting i suppose
object IOUtil {
  def expressions(fileName: String): Proof =
    scala.io.Source.fromFile(fileName).getLines().toList.map((a: String) => new ExpressionParser(a).inputLine.run().get)

  def annotatedExpressions(fileName: String): AProof =
    Annotator.annotate(expressions(fileName))

  def stringAnnotatedExpressions(fileName: String): List[String] = {
    val proof = expressions(fileName)
    Annotator.annotateString(proof, proof.foldRight(0)({(e1, e2) => math.max(e1, e2)}))
  }

  def printWithIndexes[A](list: List[A], fromIndex: Int): Unit =
    (Stream.from(fromIndex) zip list).foreach({a => println(a._1.toString + ". " + a._2.toString)})

  def getDerivation(fileName: String): Derivation = {
    val list = scala.io.Source.fromFile(fileName).getLines().toList
    (new ExpressionParser(list.head).derivationInputLine.run().get._1, list.tail.map((a: String) => new ExpressionParser(a).inputLine.run().get))
  }

  def printDerivation(derivation: Derivation): Unit =
    (List[String](derivation._1.mkString(", ") + " |- " + derivation._2.last.toString) ::: derivation._2.map(e => e.toString)).foreach(println)

  def annotateDerivation(derivation: Derivation): ADerivation = (derivation._1, Annotator.annotate(derivation._2, Annotator.contextState(derivation._1)))

  def deductionApply(d: Derivation): Derivation =
    (d._1.tail, annotateDerivation(d)._2.map {
      case (e, _) if e == d._1.head => ident(e)
      case (e, Axiom(n)) => deduction1(e, d._1.head)
      case (e, Assumption()) => deduction1(e, d._1.head)
      case (e, ModusPonens(n, m)) => deduction2(e, d._1.head, d._2(n), d._2(m)) // looks like I haven't messed up with indexes, but I'm not sure
    }.flatten)
}
