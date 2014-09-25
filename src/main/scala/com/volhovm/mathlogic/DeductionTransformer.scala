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
    val (proposals, expr0) = new ExpressionParser(curr).derivationInputLine.run().get
    while (iterator.hasNext) {
      curr = iterator.next()
      lst = new ExpressionParser(curr).inputLine.run().get :: lst
    }
    val initDerivation = Verificator.verificateRaw(lst.reverse, Verificator.mkState(proposals.map(a => (a, Assumption()))), proposals.length)
    // МЯКОТКА
  }
}
