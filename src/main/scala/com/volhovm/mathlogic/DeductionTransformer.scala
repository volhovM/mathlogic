package com.volhovm.mathlogic

/**
 * @author volhovm
 *         Created on 9/25/14
 */

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
    val initDerivation = Verificator.verificateRaw(lst.reverse, Verificator.contextState(proposals))
    initDerivation.foreach(println)
    println("\nTransforms to\n")
    var outDerivation = List[(Expr, Constr)]()
    val alpha: Expr = proposals.head
    proposals = proposals.tail
    println(proposals.mkString(", ") + " |- " + -->(alpha, initDerivation.last._2))
    val newValues = new Array[Int](initDerivation.length + 1)
    var counter = 1
    for (expr <- initDerivation) {
      expr match {
        case e if e._2 == alpha => {
          outDerivation = outDerivation ::: mkIdent(e._2, outDerivation.length)
          newValues(counter) = newValues(counter - 1) + 5
        }
        case e => e._3 match {
          case Axiom(n) =>
            outDerivation = outDerivation ::: mkAxiomOrContext(e._2, e._3, alpha, outDerivation.length)
            newValues(counter) = newValues(counter - 1) + 3
          case Assumption() =>
            outDerivation  = outDerivation ::: mkAxiomOrContext(e._2, e._3, alpha, outDerivation.length)
            newValues(counter) = newValues(counter - 1) + 3
          case ModusPonens(n, m) =>
            outDerivation  = outDerivation ::: mkMP(
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

  def mkIdent(e: Expr, line: Int): List[(Expr, Constr)] =
    List[(Expr, Constr)](
      (-->(e, -->(e, e)), Axiom(1)),
      (-->(-->(e, -->(e, e)), -->(-->(e, -->(-->(e, e), e)), -->(e, e))), Axiom(2)),
      (-->(e, -->(-->(e, e), e)), Axiom(1)),
      (-->(-->(e, -->(-->(e, e), e)), -->(e, e)), ModusPonens(line + 1, line + 2)),
      (-->(e, e), ModusPonens(line + 2, line + 3))
    )

  def mkAxiomOrContext(e: Expr, cns: Constr, alpha: Expr, line: Int): List[(Expr, Constr)] =
    List[(Expr, Constr)](
      (e, cns),
      (-->(e, --> (alpha, e)), Axiom(1)),
      (-->(alpha, e), ModusPonens(line + 1, line + 2))
    )

  def mkMP(e: Expr, alpha: Expr, expr1: Expr, n1: Int, expr2: Expr, n2: Int, line: Int) =
    List[(Expr, Constr)](
      (-->(-->(alpha, expr1), -->(-->(alpha, -->(expr1, e)), -->(alpha, e))), Axiom(2)),
      (-->(-->(alpha, -->(expr1, e)), -->(alpha, e)), ModusPonens(n1, line)),
      (-->(alpha, e), ModusPonens(n2, line + 1))
    )
}
