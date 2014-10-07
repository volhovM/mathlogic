package com.volhovm.mathlogic

import com.volhovm.mathlogic.propositional._

/**
 * @author volhovm
 *         Created on 9/25/14
 */

// TODO Ungovnocode this
object DeductionTransformer {
  def main(args: Array[String]) {
    var lst: List[Expr] = List.empty[Expr]
    val iterator = scala.io.Source.fromFile("deduction.in").getLines()
    var curr = iterator.next()
    var (proposals: List[Expr], expr0: Expr) = new ExpressionParser(curr).derivationInputLine.run().get
    while (iterator.hasNext) {
      curr = iterator.next()
      lst = new ExpressionParser(curr).inputLine.run().get :: lst
    }
    val initDerivation = Annotator.annotateLined(lst.reverse, Annotator.contextState(proposals))
    initDerivation.foreach(println)
    println("\nTransforms to\n")
    var outDerivation = List[(Expr, Constr)]()
    val alpha: Expr = proposals.head
    proposals = proposals.tail
    println(proposals.mkString(", ") + " |- " + -->(alpha, initDerivation.last._2))
    val newValues = new Array[Int](initDerivation.length + 1)
    var counter = 1
    import com.volhovm.mathlogic.propositional.Proofs._
    for (expr <- initDerivation) {
      expr match {
        case e if e._2 == alpha =>
          outDerivation = outDerivation ::: identA(e._2, outDerivation.length)
          newValues(counter) = newValues(counter - 1) + 5
        case e => e._3 match {
          case Axiom(n) =>
            outDerivation = outDerivation ::: dedLink1A(e._2, e._3, alpha, outDerivation.length)
            newValues(counter) = newValues(counter - 1) + 3
          case Assumption() =>
            outDerivation  = outDerivation ::: dedLink1A(e._2, e._3, alpha, outDerivation.length)
            newValues(counter) = newValues(counter - 1) + 3
          case ModusPonens(n, m) =>
            outDerivation  = outDerivation ::: dedLink2A(
              e._2, alpha, initDerivation(n - 1)._2, newValues(n - 1), initDerivation(m - 1)._2, newValues(m - 1), outDerivation.length)
            newValues(counter) = newValues(counter - 1) + 3
        }
      }
      counter += 1
    }
    outDerivation.foreach(a => println(a._1))
//    counter = 1
//    outDerivation.foreach(a => {println(counter.toString + ". " + a); counter += 1})
  }
}
