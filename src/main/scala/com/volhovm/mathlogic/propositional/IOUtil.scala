package com.volhovm.mathlogic.propositional

/**
 * @author volhovm
 *         Created on 10/7/14
 */

// self-documenting i suppose
object IOUtil {
  // In
  def getP(fileName: String): Proof =
    scala.io.Source.fromFile(fileName).getLines().toList.map((a: String) => new ExpressionParser(a).inputLine.run().get)

  def getAP(fileName: String): AProof =
    Annotator.annotate(getP(fileName))

  def getD(fileName: String): Derivation = {
    val list = scala.io.Source.fromFile(fileName).getLines().toList
    (new ExpressionParser(list.head).derivationInputLine.run().get._1.reverse, list.tail.map((a: String) => new ExpressionParser(a).inputLine.run().get))
  }

  def getAD(fileName: String): ADerivation =
    Annotator.annotateDerivation(getD(fileName))


  // Out
  def printP(proof: Proof) = proof.foreach(println)
  def printAP(proof: AProof) = {
    val margin = proof.foldRight(0)((e, n) => math.max(e._1, n))
    (Stream.from(1) zip proof).foreach(e => println((e._1 + ". %-" + (margin + 10) + "s%-20s").format(e._2._1, e._2._2)))
  }

  def header[A](derivation: (List[A], List[A])): String = derivation._1.reverse.mkString(", ") + " |- " + derivation._2.last.toString

  def printD(derivation: Derivation): Unit = {
    println(header(derivation))
    printP(derivation._2)
  }

  def printAD(derivation: ADerivation): Unit = {
    println(header(derivation))
    printAP(derivation._2)
  }

  // Performance
  def stringAnnotatedExpressions(fileName: String): List[String] = {
    val proof = getP(fileName)
    Annotator.annotateGeneric(proof, Annotator.emptyState[String],
    {(x, c, l) => (l + ". %-" + (proof.foldRight(0)(math.max(_, _)) + 10) + "s%-20s").format(x, c)}
    )
  }
}
